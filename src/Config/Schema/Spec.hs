{-# LANGUAGE RankNTypes, GADTs, KindSignatures, GeneralizedNewtypeDeriving #-}

{-|
Module      : Config.Schema.Spec
Description : 
Copyright   : (c) Eric Mertens, 2017
License     : ISC
Maintainer  : emertens@gmail.com

-}
module Config.Schema.Spec where

import Config
import Control.Applicative              (optional, (<|>))
import Control.Applicative.Free         (Ap, runAp, runAp_, liftAp)
import Data.Monoid                      ((<>))
import Data.Text                        (Text)
import qualified Data.Text as Text

------------------------------------------------------------------------
-- Configuration format description
------------------------------------------------------------------------

newtype SectionsSpec a = MkSectionsSpec (Ap SectionSpec a)
  deriving (Functor, Applicative)


sectionsSpec :: SectionSpec a -> SectionsSpec a
sectionsSpec = MkSectionsSpec . liftAp


runSections :: Applicative f => (forall x. SectionSpec x -> f x) -> SectionsSpec a -> f a
runSections f (MkSectionsSpec s) = runAp f s


runSections_ :: Monoid m => (forall x. SectionSpec x -> m) -> SectionsSpec a -> m
runSections_ f (MkSectionsSpec s) = runAp_ f s


data SectionSpec :: * -> * where
  ReqSection :: Text -> Text -> ValueSpec a -> SectionSpec a
  OptSection :: Text -> Text -> ValueSpec a -> SectionSpec (Maybe a)

data ValueSpec :: * -> * where
  ListSpec     :: ValueSpec a -> ValueSpec [a]
  TextSpec     :: ValueSpec Text
  NumberSpec   :: ValueSpec Integer
  AtomSpec     :: Atom -> ValueSpec ()
  ChoiceSpec   :: ValueSpec a -> ValueSpec b -> ValueSpec (Either a b)
  SectionsSpec :: SectionsSpec a -> ValueSpec a
  MapSpec      :: (a -> b) -> ValueSpec a -> ValueSpec b


-- | Class for things that are easy to want
class    Spec a       where valueSpec :: ValueSpec a
instance Spec Text    where valueSpec = TextSpec
instance Spec Integer where valueSpec = NumberSpec
instance Spec a => Spec [a] where valueSpec = ListSpec valueSpec
instance (Spec a, Spec b) => Spec (Either a b) where valueSpec = ChoiceSpec valueSpec valueSpec


section ::
  Spec a =>
  Text {- ^ section name -} ->
  Text {- ^ description  -} ->
  SectionsSpec a
section n i = sectionsSpec (ReqSection n i valueSpec)


section' ::
  Text        {- ^ section name  -} ->
  Text        {- ^ description   -} ->
  ValueSpec a {- ^ value matcher -} ->
  SectionsSpec a
section' n i w = sectionsSpec (ReqSection n i w)


------------------------------------------------------------------------
-- Configuration value parsing
------------------------------------------------------------------------


------------------------------------------------------------------------
-- Documentation generation
------------------------------------------------------------------------


