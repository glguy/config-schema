{-# Language OverloadedStrings, GeneralizedNewtypeDeriving, GADTs #-}
{-|
Module      : Config.Schema.Load
Description : Operations to extract a value from a configuration.
Copyright   : (c) Eric Mertens, 2017
License     : ISC
Maintainer  : emertens@gmail.com

This module automates the extraction of a decoded value from a configuration
value according to a specification as built using "Config.Schema.Spec".

-}
module Config.Schema.Load
  ( loadValue
  , loadValueFromFile

  -- * Errors
  , SchemaError(..)
  , LoadError(..)
  , Problem(..)
  ) where

import           Control.Exception                (Exception(..), throwIO)
import           Control.Monad                    (zipWithM)
import           Control.Monad.Trans.Class        (lift)
import           Control.Monad.Trans.State        (StateT(..), runStateT)
import           Control.Monad.Trans.Except       (Except, runExcept, throwE)
import           Control.Monad.Trans.Reader       (ReaderT, runReaderT, ask, local)
import           Data.Semigroup.Foldable          (asum1)
import           Data.Functor.Alt                 (Alt((<!>)))
import           Data.Monoid                      ((<>))
import           Data.Ratio                       (numerator, denominator)
import           Data.List.NonEmpty               (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Text                        (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import           Config
import           Config.Schema.Spec


-- | Match a 'Value' against a 'ValueSpecs' and return either
-- the interpretation of that value or the list of errors
-- encountered.
loadValue ::
  ValueSpecs a                      {- ^ specification           -} ->
  Value p                           {- ^ value                   -} ->
  Either (NonEmpty (LoadError p)) a {- ^ errors or decoded value -}
loadValue spec val = runLoad (getValue spec val)


-- | Read a configuration file, parse it, and validate it according
-- to the given specification.
--
-- Throws 'IOError', 'ParseError', or @'NonEmpty' ('LoadError' 'Position')@
loadValueFromFile ::
  FilePath     {- ^ filename      -} ->
  ValueSpecs a {- ^ specification -} ->
  IO a
loadValueFromFile path spec =
  do txt <- Text.readFile path
     val <- either throwIO return (parse txt)
     either (throwIO . SchemaError) return (loadValue spec val)


-- | Newtype wrapper for schema load errors.
newtype SchemaError = SchemaError (NonEmpty (LoadError Position))
  deriving Show

-- | Custom 'displayException' implementation
instance Exception SchemaError where
  displayException (SchemaError e) = foldr showLoadError "" e
    where
      showLoadError (LoadError pos path problem)
        = shows (posLine pos)
        . showChar ':'
        . shows (posColumn pos)
        . showString ": "
        . foldr (\x xs -> showString (Text.unpack x) . showChar ':' . xs) id path
        . showChar ' '
        . showProblem problem
        . showChar '\n'

      showProblem p =
        case p of
          MissingSection x -> showString "missing required section `"
                            . showString (Text.unpack x) . showChar '`'
          UnusedSection  x -> showString "unused section `"
                            . showString (Text.unpack x) . showChar '`'
          SpecMismatch   x -> showString "expected " . showString (Text.unpack x)


getSection :: p -> SectionSpec a -> StateT [Section p] (Load p) a
getSection pos (ReqSection k _ w) =
  do v <- StateT (lookupSection pos k)
     lift (scope k (getValue w v))
getSection pos (OptSection k _ w) =
  do mb <- optional1 (StateT (lookupSection pos k))
     lift (traverse (scope k . getValue w) mb)


getSections :: p -> SectionSpecs a -> [Section p] -> Load p a
getSections pos spec xs =
  do (a,leftovers) <- runStateT (runSections (getSection pos) spec) xs
     case NonEmpty.nonEmpty leftovers of
       Nothing -> return a
       Just ss -> asum1 (fmap (\s -> loadFail (sectionAnn s) (UnusedSection (sectionName s))) ss)


getValue :: ValueSpecs a -> Value p -> Load p a
getValue s v = runValueSpecs (getValue1 v) s


-- | Match a primitive value specification against a single value.
getValue1 :: Value p -> ValueSpec a -> Load p a
getValue1 (Text _ t)       TextSpec           = pure t
getValue1 (Number _ _ n)   IntegerSpec        = pure n
getValue1 (Floating _ a b) IntegerSpec | Just i <- floatingToInteger a b = pure i
getValue1 (Number _ _ n)   RationalSpec       = pure (fromInteger n)
getValue1 (Floating _ a b) RationalSpec       = pure (floatingToRational a b)
getValue1 (List _ xs)      (ListSpec w)       = getList w xs
getValue1 (Atom _ b)       AnyAtomSpec        = pure (atomName b)
getValue1 (Atom _ b)       (AtomSpec a) | a == atomName b = pure ()
getValue1 (Sections p s)   (SectionSpecs _ w) = getSections p w s
getValue1 (Sections _ s)   (AssocSpec w)      = getAssoc w s
getValue1 v                (NamedSpec _ w)    = getValue w v
getValue1 v                (CustomSpec l w)   = getCustom l w v

getValue1 v TextSpec           = loadFail (valueAnn v) (SpecMismatch "text")
getValue1 v IntegerSpec        = loadFail (valueAnn v) (SpecMismatch "integer")
getValue1 v RationalSpec       = loadFail (valueAnn v) (SpecMismatch "number")
getValue1 v ListSpec{}         = loadFail (valueAnn v) (SpecMismatch "list")
getValue1 v AnyAtomSpec        = loadFail (valueAnn v) (SpecMismatch "atom")
getValue1 v (AtomSpec a)       = loadFail (valueAnn v) (SpecMismatch ("`" <> a <> "`"))
getValue1 v (SectionSpecs l _) = loadFail (valueAnn v) (SpecMismatch l)
getValue1 v AssocSpec{}        = loadFail (valueAnn v) (SpecMismatch "association list")


-- | This operation processes all of the values in a list with the given
-- value specification and updates the scope with a one-based list index.
getList :: ValueSpecs a -> [Value p] -> Load p [a]
getList w = zipWithM (\i x -> scope (Text.pack (show i)) (getValue w x)) [1::Int ..]


-- | This operation processes all of the values in a section list
-- against the given specification and associates them with the
-- section name.
getAssoc :: ValueSpecs a -> [Section p] -> Load p [(Text,a)]
getAssoc w = traverse $ \(Section _ k v) -> (,) k <$> scope k (getValue w v)


-- | Match a value against its specification. If 'Just' is matched
-- return the value. If 'Nothing is matched, report an error.
getCustom ::
  Text                 {- ^ label         -} ->
  ValueSpecs (Maybe a) {- ^ specification -} ->
  Value p              {- ^ value         -} ->
  Load p a
getCustom l w v =
  do x <- getValue w v
     case x of
       Nothing -> loadFail (valueAnn v) (SpecMismatch l)
       Just y  -> pure y


-- | Extract a section from a list of sections by name.
lookupSection ::
  p                             {- ^ starting position of sections      -} ->
  Text                          {- ^ section name                       -} ->
  [Section p]                   {- ^ available sections                 -} ->
  Load p (Value p, [Section p]) {- ^ found value and remaining sections -}
lookupSection pos key [] = loadFail pos (MissingSection key)
lookupSection pos key (s@(Section _ k v):xs)
  | key == k  = pure (v, xs)
  | otherwise = do (v',xs') <- lookupSection pos key xs
                   return (v',s:xs')

------------------------------------------------------------------------

-- | Interpret a @config-value@ floating point number as a 'Rational'.
floatingToRational :: Integer -> Integer -> Rational
floatingToRational x y = fromInteger x * 10^^y

-- | Interpret a @config-value@ floating point number as an 'Integer'
-- if possible.
floatingToInteger :: Integer -> Integer -> Maybe Integer
floatingToInteger x y
  | denominator r == 1 = Just (numerator r)
  | otherwise          = Nothing
  where r = floatingToRational x y

------------------------------------------------------------------------
-- Error reporting type
------------------------------------------------------------------------


-- | Type used to match values against specifiations. This type tracks
-- the current nested fields (updated with scope) and can throw
-- errors using loadFail.
newtype Load p a = MkLoad { unLoad :: ReaderT [Text] (Except (NonEmpty (LoadError p))) a }
  deriving (Functor, Applicative, Monad)

instance Alt (Load p) where MkLoad x <!> MkLoad y = MkLoad (x <!> y)

-- | Type for errors that can be encountered while decoding a value according
-- to a specification. The error includes a key path indicating where in
-- the configuration file the error occurred.
data LoadError p = LoadError p [Text] Problem -- ^ position, path, problem
  deriving (Read, Show)


-- | Run the Load computation until it produces a result or terminates
-- with a list of errors.
runLoad :: Load p a -> Either (NonEmpty (LoadError p)) a
runLoad = runExcept . flip runReaderT [] . unLoad


-- | Problems that can be encountered when matching a 'Value' against a 'ValueSpecs'.
data Problem
  = MissingSection Text -- ^ missing section name
  | UnusedSection Text  -- ^ unused section names
  | SpecMismatch Text   -- ^ failed specification name
  deriving (Eq, Ord, Read, Show)

-- | Push a new key onto the stack of nested fields.
scope :: Text -> Load p a -> Load p a
scope key (MkLoad m) = MkLoad (local (key:) m)

-- | Abort value specification matching with the given error.
loadFail :: p -> Problem -> Load p a
loadFail pos cause = MkLoad $
  do path <- ask
     lift (throwE (pure (LoadError pos (reverse path) cause)))

------------------------------------------------------------------------

-- | One or none. This definition is different from the normal @optional@ definition
-- because it uses 'Alt'. This allows it to work on types that are not @Alternative@.
optional1 :: (Applicative f, Alt f) => f a -> f (Maybe a)
optional1 fa = Just <$> fa <!> pure Nothing
