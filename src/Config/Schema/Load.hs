{-# Language GeneralizedNewtypeDeriving, GADTs #-}
{-|
Module      : Config.Schema.Load
Description : 
Copyright   : (c) Eric Mertens, 2017
License     : ISC
Maintainer  : emertens@gmail.com

-}
module Config.Schema.Load where

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


loadSections :: SectionsSpec a -> Value -> Either [LoadError] a
loadSections spec val = runLoad (getValue (SectionsSpec spec) val)


getSection :: SectionSpec a -> StateT [Section] Load a
getSection (ReqSection k _ w) =
  do v <- StateT (lookupSection k)
     lift (scope k (getValues w v))
getSection (OptSection k _ w) =
  do mb <- optional (StateT (lookupSection k))
     lift (traverse (scope k . getValues w) mb)


getSections :: SectionsSpec a -> [Section] -> Load a
getSections p xs =
  do (a,leftover) <- runStateT (runSections getSection p) xs
     unless (null leftover) (loadFail (UnusedSections (map sectionName leftover)))
     return a


getValues :: ValuesSpec a -> Value -> Load a
getValues s v = asum (runValuesSpec (`getValue` v) s)


getValue :: ValueSpec a -> Value -> Load a
getValue TextSpec         (Text t)       = pure t
getValue IntegerSpec      (Number _ n)   = pure n
getValue IntegerSpec      (Floating a b) = maybe (loadFail TypeMismatch) pure (floatingToInteger a b)
getValue RationalSpec     (Number _ n)   = pure (fromInteger n)
getValue RationalSpec     (Floating a b) = pure (floatingToRational a b)
getValue (ListSpec w)     (List xs)      = scopedTraverse (getValues w) xs
getValue AnyAtomSpec      (Atom b)       = pure (atomName b)
getValue (AtomSpec a)     (Atom b)       = unless (a == atomName b) (loadFail (AtomMismatch a))
getValue (SectionsSpec w) (Sections s)   = getSections w s
getValue (CustomSpec lbl w f) v = do x <- getValues w v
                                     case f x of
                                       Nothing -> loadFail (CustomFail lbl)
                                       Just y  -> pure y
getValue _                _              = loadFail TypeMismatch


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
