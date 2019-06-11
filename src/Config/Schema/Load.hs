{-# Language GADTs #-}
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
  , ValueSpecMismatch(..)
  , PrimMismatch(..)
  , Problem(..)
  ) where

import           Control.Exception                (throwIO)
import           Control.Monad                    (zipWithM)
import           Control.Monad.Trans.Class        (lift)
import           Control.Monad.Trans.State        (StateT(..), runStateT, state)
import           Control.Monad.Trans.Except       (Except, runExcept, throwE, withExcept)
import           Data.Ratio                       (numerator, denominator)
import           Data.List.NonEmpty               (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Text                        (Text)
import qualified Data.Text.IO as Text

import           Config
import           Config.Schema.Types
import           Config.Schema.Load.Error


-- | Match a 'Value' against a 'ValueSpec' and return either
-- the interpretation of that value or the list of errors
-- encountered.
loadValue ::
  ValueSpec a                       {- ^ specification           -} ->
  Value p                           {- ^ value                   -} ->
  Either (ValueSpecMismatch p) a {- ^ errors or decoded value -}
loadValue spec val = runExcept (getValue spec val)


-- | Read a configuration file, parse it, and validate it according
-- to the given specification.
--
-- Throws 'IOError', 'ParseError', or 'ValueSpecMismatch'
loadValueFromFile ::
  ValueSpec a {- ^ specification -} ->
  FilePath    {- ^ filename      -} ->
  IO a
loadValueFromFile spec path =
  do txt <- Text.readFile path
     let exceptIO m = either throwIO return m
     val <- exceptIO (parse txt)
     exceptIO (loadValue spec val)

getSection :: PrimSectionSpec a -> StateT [Section p] (Except (Problem p)) a
getSection (ReqSection k _ w) =
  do mb <- state (lookupSection k)
     lift $ case mb of
       Just v -> getValue' (SubkeyProblem k) w v
       Nothing -> throwE (MissingSection k)
getSection (OptSection k _ w) =
  do mb <- state (lookupSection k)
     lift (traverse (getValue' (SubkeyProblem k) w) mb)


getSections :: SectionsSpec a -> [Section p] -> Except (Problem p) a
getSections spec xs =
  do (a,leftovers) <- runStateT (runSections getSection spec) xs
     case NonEmpty.nonEmpty leftovers of
       Nothing -> return a
       Just ss -> throwE (UnusedSections (fmap sectionName ss))


getValue :: ValueSpec a -> Value p -> Except (ValueSpecMismatch p) a
getValue s v = withExcept (ValueSpecMismatch (valueAnn v) (describeValue v)) (runValueSpec (getValue1 v) s)

-- | Match a 'Value' against a 'ValueSpec' given a wrapper for any nested
-- mismatch errors that might occur.
getValue' ::
  (ValueSpecMismatch p -> Problem p) ->
  ValueSpec a ->
  Value p ->
  Except (Problem p) a
getValue' p s v = withExcept (p . ValueSpecMismatch (valueAnn v) (describeValue v)) (runValueSpec (getValue1 v) s)

getValue1 :: Value p -> PrimValueSpec a -> Except (NonEmpty (PrimMismatch p)) a
getValue1 v prim = withExcept (pure . PrimMismatch (describeSpec prim))
                              (getValue2 v prim)

-- | Match a primitive value specification against a single value.
getValue2 :: Value p -> PrimValueSpec a -> Except (Problem p) a
getValue2 (Text _ t)       TextSpec           = pure t
getValue2 (Number _ _ n)   IntegerSpec        = pure n
getValue2 (Floating _ a b) IntegerSpec | Just i <- floatingToInteger a b = pure i
getValue2 (Number _ _ n)   RationalSpec       = pure (fromInteger n)
getValue2 (Floating _ a b) RationalSpec       = pure (floatingToRational a b)
getValue2 (List _ xs)      (ListSpec w)       = getList w xs
getValue2 (Atom _ b)       AnyAtomSpec        = pure (atomName b)
getValue2 (Atom _ b)       (AtomSpec a)
  | a == atomName b = pure ()
  | otherwise       = throwE WrongAtom
getValue2 (Sections _ s)   (SectionsSpec _ w) = getSections w s
getValue2 (Sections _ s)   (AssocSpec w)      = getAssoc w s
getValue2 v                (NamedSpec _ w)    = getValue' NestedProblem w v
getValue2 v                (CustomSpec _ w)   = getCustom w v
getValue2 _                _                  = throwE TypeMismatch


-- | This operation processes all of the values in a list with the given
-- value specification and updates the scope with a one-based list index.
getList :: ValueSpec a -> [Value p] -> Except (Problem p) [a]
getList w = zipWithM (\i -> getValue' (ListElementProblem i) w) [1::Int ..]


-- | This operation processes all of the values in a section list
-- against the given specification and associates them with the
-- section name.
getAssoc :: ValueSpec a -> [Section p] -> Except (Problem p) [(Text,a)]
getAssoc w = traverse $ \(Section _ k v) ->
                 (,) k <$> getValue' (SubkeyProblem k) w v

-- | Match a value against its specification. If 'Just' is matched
-- return the value. If 'Nothing is matched, report an error.
getCustom ::
  ValueSpec (Either Text a) {- ^ specification -} ->
  Value p                   {- ^ value         -} ->
  Except (Problem p) a
getCustom w v = either (throwE . CustomProblem) pure =<< getValue' NestedProblem w v


-- | Extract a section from a list of sections by name.
lookupSection ::
  Text                         {- ^ section name                       -} ->
  [Section p]                  {- ^ available sections                 -} ->
  (Maybe (Value p), [Section p]) {- ^ found value and remaining sections -}
lookupSection _ [] = (Nothing, [])
lookupSection key (s@(Section _ k v):xs)
  | key == k  = (Just v, xs)
  | otherwise = case lookupSection key xs of
                  (res, xs') -> (res, s:xs')

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
