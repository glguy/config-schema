{-# Language GeneralizedNewtypeDeriving, GADTs #-}
{-|
Module      : Config.Schema.Load
Description : 
Copyright   : (c) Eric Mertens, 2017
License     : ISC
Maintainer  : emertens@gmail.com

-}
module Config.Schema.Load
  ( loadValue

  -- * Errors
  , LoadError(..)
  , Problem(..)
  ) where

import           Control.Applicative              (Alternative, optional)
import           Control.Monad                    (MonadPlus, unless, zipWithM)
import           Control.Monad.Trans.Class        (lift)
import           Control.Monad.Trans.State        (StateT(..), runStateT)
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Reader
import           Data.Foldable                    (asum)
import           Data.Ratio                       (numerator, denominator)
import           Data.Text                        (Text)
import qualified Data.Text as Text

import           Config
import           Config.Schema.Spec


loadValue :: ValueSpecs a -> Value -> Either [LoadError] a
loadValue spec val = runLoad (getValue spec val)


getSection :: SectionSpec a -> StateT [Section] Load a
getSection (ReqSection k _ w) =
  do v <- StateT (lookupSection k)
     lift (scope k (getValue w v))
getSection (OptSection k _ w) =
  do mb <- optional (StateT (lookupSection k))
     lift (traverse (scope k . getValue w) mb)


getSections :: SectionSpecs a -> [Section] -> Load a
getSections p xs =
  do (a,leftover) <- runStateT (runSections getSection p) xs
     unless (null leftover) (loadFail (UnusedSections (map sectionName leftover)))
     return a


getValue :: ValueSpecs a -> Value -> Load a
getValue s v = asum (runValueSpecs (getValue1 v) s)


getValue1 :: Value -> ValueSpec a -> Load a
getValue1 (Text t)       TextSpec           = pure t
getValue1 (Number _ n)   IntegerSpec        = pure n
getValue1 (Floating a b) IntegerSpec        = maybe (loadFail TypeMismatch) pure (floatingToInteger a b)
getValue1 (Number _ n)   RationalSpec       = pure (fromInteger n)
getValue1 (Floating a b) RationalSpec       = pure (floatingToRational a b)
getValue1 (List xs)      (ListSpec w)       = scopedTraverse (getValue w) xs
getValue1 (Atom b)       AnyAtomSpec        = pure (atomName b)
getValue1 (Atom b)       (AtomSpec a)       = unless (a == atomName b) (loadFail (AtomMismatch a))
getValue1 (Sections s)   (SectionSpecs _ w) = getSections w s
getValue1 v              (CustomSpec l w f) =
  do x <- getValue w v
     case f x of
       Nothing -> loadFail (CustomFail l)
       Just y  -> pure y
getValue1 _                  _              = loadFail TypeMismatch


scopedTraverse :: (a -> Load b) -> [a] -> Load [b]
scopedTraverse f = zipWithM (\i x -> scope (Text.pack (show i)) (f x)) [1::Int ..]


-- | Extract a section from a list of sections by name.
lookupSection ::
  Text                     {- ^ section name                       -} ->
  [Section]                {- ^ available sections                 -} ->
  Load (Value, [Section]) {- ^ found value and remaining sections -}
lookupSection key [] = loadFail (MissingSection key)
lookupSection key (s@(Section k v):xs)
  | key == k  = pure (v, xs)
  | otherwise = do (v',xs') <- lookupSection key xs
                   return (v',s:xs')

------------------------------------------------------------------------

floatingToRational :: Integer -> Integer -> Rational
floatingToRational x y = fromInteger x * 10^^y

floatingToInteger :: Integer -> Integer -> Maybe Integer
floatingToInteger x y
  | denominator r == 1 = Just (numerator r)
  | otherwise          = Nothing
  where r = floatingToRational x y

------------------------------------------------------------------------
-- Error reporting type
------------------------------------------------------------------------

newtype Load a = MkLoad { unLoad :: ReaderT [Text] (Except [LoadError]) a }
  deriving (Functor, Applicative, Monad, Alternative, MonadPlus)

data LoadError = LoadError [Text] Problem
  deriving (Eq, Ord, Read, Show)

data Problem
  = AtomMismatch Text -- ^ expected atom
  | MissingSection Text -- ^ section name
  | TypeMismatch
  | UnusedSections [Text] -- ^ unused section names
  | CustomFail Text
  deriving (Eq, Ord, Read, Show)

scope :: Text -> Load a -> Load a
scope key (MkLoad m) = MkLoad (local (key:) m)

loadFail :: Problem -> Load a
loadFail cause = MkLoad $
  do path <- ask
     lift (throwE [LoadError (reverse path) cause])

runLoad :: Load a -> Either [LoadError] a
runLoad = runExcept . flip runReaderT [] . unLoad
