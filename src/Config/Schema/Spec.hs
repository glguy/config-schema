{-# LANGUAGE FlexibleInstances, RankNTypes, GADTs, KindSignatures, GeneralizedNewtypeDeriving #-}

{-|
Module      : Config.Schema.Spec
Description : 
Copyright   : (c) Eric Mertens, 2017
License     : ISC
Maintainer  : emertens@gmail.com

-}
module Config.Schema.Spec where

import           Control.Applicative              (Alternative(..))
import           Control.Applicative.Free         (Ap, runAp, runAp_, liftAp)
import           Data.Functor.Compose
import           Data.Text                        (Text)
import qualified Data.Text as Text

------------------------------------------------------------------------
-- Specifications for sections
------------------------------------------------------------------------

data SectionSpec :: * -> * where
  ReqSection :: Text -> Text -> ValuesSpec a -> SectionSpec a
  OptSection :: Text -> Text -> ValuesSpec a -> SectionSpec (Maybe a)


newtype SectionsSpec a = MkSectionsSpec (Ap SectionSpec a)
  deriving (Functor, Applicative)


sectionSpec :: SectionSpec a -> SectionsSpec a
sectionSpec = MkSectionsSpec . liftAp


runSections :: Applicative f => (forall x. SectionSpec x -> f x) -> SectionsSpec a -> f a
runSections f (MkSectionsSpec s) = runAp f s


runSections_ :: Monoid m => (forall x. SectionSpec x -> m) -> SectionsSpec a -> m
runSections_ f (MkSectionsSpec s) = runAp_ f s


------------------------------------------------------------------------
-- 'SectionsSpec' builders
------------------------------------------------------------------------


reqSection ::
  Spec a =>
  Text {- ^ section name -} ->
  Text {- ^ description  -} ->
  SectionsSpec a
reqSection n i = sectionSpec (ReqSection n i valuesSpec)


reqSection' ::
  Text         {- ^ section name  -} ->
  Text         {- ^ description   -} ->
  ValuesSpec a {- ^ value matcher -} ->
  SectionsSpec a
reqSection' n i w = sectionSpec (ReqSection n i w)


optSection ::
  Spec a =>
  Text {- ^ section name -} ->
  Text {- ^ description  -} ->
  SectionsSpec (Maybe a)
optSection n i = sectionSpec (OptSection n i valuesSpec)


optSection' ::
  Text         {- ^ section name  -} ->
  Text         {- ^ description   -} ->
  ValuesSpec a {- ^ value matcher -} ->
  SectionsSpec (Maybe a)
optSection' n i w = sectionSpec (OptSection n i w)


------------------------------------------------------------------------
-- Specifications for values
------------------------------------------------------------------------

data ValueSpec :: * -> * where
  TextSpec     ::                   ValueSpec Text
  IntegerSpec  ::                   ValueSpec Integer
  RationalSpec ::                   ValueSpec Rational
  AnyAtomSpec  ::                   ValueSpec Text
  AtomSpec     :: Text           -> ValueSpec ()
  ListSpec     :: ValuesSpec   a -> ValueSpec [a]
  SectionsSpec :: SectionsSpec a -> ValueSpec a
  CustomSpec   :: Text -> ValuesSpec a -> (a -> Maybe b) -> ValueSpec b


newtype ValuesSpec a = MkValuesSpec { unValueSpec :: Compose [] (Ap ValueSpec) a }
  deriving (Functor, Applicative, Alternative)


runValuesSpec :: Applicative f => (forall x. ValueSpec x -> f x) -> ValuesSpec a -> [f a]
runValuesSpec f =  map (runAp f) . getCompose . unValueSpec

runValuesSpec_ :: Monoid m => (forall x. ValueSpec x -> m) -> ValuesSpec a -> [m]
runValuesSpec_ f = map (runAp_ f) . getCompose . unValueSpec


valueSpec :: ValueSpec a -> ValuesSpec a
valueSpec = MkValuesSpec . Compose . pure . liftAp


------------------------------------------------------------------------
-- 'ValuesSpec' builders
------------------------------------------------------------------------


-- | Class for things that are easy to want
class    Spec a       where valuesSpec :: ValuesSpec a
instance Spec Text    where valuesSpec = valueSpec TextSpec
instance Spec Integer where valuesSpec = valueSpec IntegerSpec
instance Spec Int     where valuesSpec = fromInteger <$> valuesSpec
instance Spec Rational where valuesSpec = valueSpec RationalSpec
instance Spec a => Spec [a] where valuesSpec = valueSpec (ListSpec valuesSpec)
instance (Spec a, Spec b) => Spec (Either a b) where valuesSpec = valuesSpec <|> valuesSpec


atomSpec :: Text -> ValuesSpec ()
atomSpec = valueSpec . AtomSpec

anyAtomSpec :: ValuesSpec Text
anyAtomSpec = valueSpec AnyAtomSpec

stringSpec :: ValuesSpec String
stringSpec = Text.unpack <$> valuesSpec

numSpec :: Num a => ValuesSpec a
numSpec = fromInteger <$> valuesSpec

listSpec :: ValuesSpec a -> ValuesSpec [a]
listSpec = valueSpec . ListSpec


sectionsSpec :: SectionsSpec a -> ValuesSpec a
sectionsSpec = valueSpec . SectionsSpec


oneOrList :: ValuesSpec a -> ValuesSpec [a]
oneOrList s = (:[]) <$> s <|> listSpec s


customSpec :: Text -> ValuesSpec a -> (a -> Maybe b) -> ValuesSpec b
customSpec lbl w f = valueSpec (CustomSpec lbl w f)
