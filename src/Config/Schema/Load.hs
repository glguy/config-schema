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

  -- * Errors
  , LoadError(..)
  , Problem(..)
  ) where

import           Control.Monad                    (unless, zipWithM)
import           Control.Monad.Trans.Class        (lift)
import           Control.Monad.Trans.State        (StateT(..), runStateT)
import           Control.Monad.Trans.Except       (Except, runExcept, throwE)
import           Control.Monad.Trans.Reader       (ReaderT, runReaderT, ask, local)
import           Data.Functor.Alt                 (Alt((<!>)))
import           Data.Monoid                      ((<>))
import           Data.Ratio                       (numerator, denominator)
import           Data.Semigroup.Foldable          (asum1)
import           Data.List.NonEmpty               (NonEmpty)
import           Data.Text                        (Text)
import qualified Data.Text as Text

import           Config
import           Config.Schema.Spec


-- | Match a 'Value' against a 'ValueSpecs' and return either
-- the interpretation of that value or the list of errors
-- encountered.
loadValue ::
  ValueSpecs a                  {- ^ specification           -} ->
  Value                         {- ^ value                   -} ->
  Either (NonEmpty LoadError) a {- ^ errors or decoded value -}
loadValue spec val = runLoad (getValue spec val)


getSection :: SectionSpec a -> StateT [Section] Load a
getSection (ReqSection k _ w) =
  do v <- StateT (lookupSection k)
     lift (scope k (getValue w v))
getSection (OptSection k _ w) =
  do mb <- optional1 (StateT (lookupSection k))
     lift (traverse (scope k . getValue w) mb)


getSections :: SectionSpecs a -> [Section] -> Load a
getSections p xs =
  do (a,leftover) <- runStateT (runSections getSection p) xs
     unless (null leftover) (loadFail (UnusedSections (map sectionName leftover)))
     return a


getValue :: ValueSpecs a -> Value -> Load a
getValue s v = asum1 (runValueSpecs (getValue1 v) s)


-- | Match a primitive value specification against a single value.
getValue1 :: Value -> ValueSpec a -> Load a
getValue1 (Text t)       TextSpec           = pure t
getValue1 (Number _ n)   IntegerSpec        = pure n
getValue1 (Floating a b) IntegerSpec | Just i <- floatingToInteger a b = pure i
getValue1 (Number _ n)   RationalSpec       = pure (fromInteger n)
getValue1 (Floating a b) RationalSpec       = pure (floatingToRational a b)
getValue1 (List xs)      (ListSpec w)       = getList w xs
getValue1 (Atom b)       AnyAtomSpec        = pure (atomName b)
getValue1 (Atom b)       (AtomSpec a) | a == atomName b = pure ()
getValue1 (Sections s)   (SectionSpecs _ w) = getSections w s
getValue1 (Sections s)   (AssocSpec w)      = getAssoc w s
getValue1 v              (NamedSpec _ w)    = getValue w v
getValue1 v              (CustomSpec l w)   = getCustom l w v

getValue1 _              TextSpec           = loadFail (SpecMismatch "text")
getValue1 _              IntegerSpec        = loadFail (SpecMismatch "integer")
getValue1 _              RationalSpec       = loadFail (SpecMismatch "number")
getValue1 _              ListSpec{}         = loadFail (SpecMismatch "list")
getValue1 _              AnyAtomSpec        = loadFail (SpecMismatch "atom")
getValue1 _              (AtomSpec a)       = loadFail (SpecMismatch ("`" <> a <> "`"))
getValue1 _              (SectionSpecs l _) = loadFail (SpecMismatch l)
getValue1 _              AssocSpec{}        = loadFail (SpecMismatch "association list")


-- | This operation processes all of the values in a list with the given
-- value specification and updates the scope with a one-based list index.
getList :: ValueSpecs a -> [Value] -> Load [a]
getList w = zipWithM (\i x -> scope (Text.pack (show i)) (getValue w x)) [1::Int ..]


-- | This operation processes all of the values in a section list
-- against the given specification and associates them with the
-- section name.
getAssoc :: ValueSpecs a -> [Section] -> Load [(Text,a)]
getAssoc w = traverse $ \(Section k v) -> (,) k <$> getValue w v


-- | Match a value against its specification. If 'Just' is matched
-- return the value. If 'Nothing is matched, report an error.
getCustom ::
  Text                 {- ^ label         -} ->
  ValueSpecs (Maybe a) {- ^ specification -} ->
  Value                {- ^ value         -} ->
  Load a
getCustom l w v =
  do x <- getValue w v
     case x of
       Nothing -> loadFail (SpecMismatch l)
       Just y  -> pure y


-- | Extract a section from a list of sections by name.
lookupSection ::
  Text                    {- ^ section name                       -} ->
  [Section]               {- ^ available sections                 -} ->
  Load (Value, [Section]) {- ^ found value and remaining sections -}
lookupSection key [] = loadFail (MissingSection key)
lookupSection key (s@(Section k v):xs)
  | key == k  = pure (v, xs)
  | otherwise = do (v',xs') <- lookupSection key xs
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
newtype Load a = MkLoad { unLoad :: ReaderT [Text] (Except (NonEmpty LoadError)) a }
  deriving (Functor, Applicative, Monad)

instance Alt Load where MkLoad x <!> MkLoad y = MkLoad (x <!> y)

-- | Type for errors that can be encountered while decoding a value according
-- to a specification. The error includes a key path indicating where in
-- the configuration file the error occurred.
data LoadError = LoadError [Text] Problem -- ^ path to problem and problem description
  deriving (Eq, Ord, Read, Show)


-- | Run the Load computation until it produces a result or terminates
-- with a list of errors.
runLoad :: Load a -> Either (NonEmpty LoadError) a
runLoad = runExcept . flip runReaderT [] . unLoad


-- | Problems that can be encountered when matching a 'Value' against a 'ValueSpecs'.
data Problem
  = MissingSection Text   -- ^ missing section name
  | UnusedSections [Text] -- ^ unused section names
  | SpecMismatch Text     -- ^ failed specification name
  deriving (Eq, Ord, Read, Show)

-- | Push a new key onto the stack of nested fields.
scope :: Text -> Load a -> Load a
scope key (MkLoad m) = MkLoad (local (key:) m)

-- | Abort value specification matching with the given error.
loadFail :: Problem -> Load a
loadFail cause = MkLoad $
  do path <- ask
     lift (throwE (pure (LoadError (reverse path) cause)))

------------------------------------------------------------------------

-- | One or none. This definition is different from the normal @optional@ definition
-- because it uses 'Alt'. This allows it to work on types that are not @Alternative@.
optional1 :: (Applicative f, Alt f) => f a -> f (Maybe a)
optional1 fa = Just <$> fa <!> pure Nothing
