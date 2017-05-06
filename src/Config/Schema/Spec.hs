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
  ReqSection :: Text -> Text -> ValueSpecs a -> SectionSpec a
  OptSection :: Text -> Text -> ValueSpecs a -> SectionSpec (Maybe a)


newtype SectionSpecs a = MkSectionSpecs (Ap SectionSpec a)
  deriving (Functor, Applicative)


sectionSpec :: SectionSpec a -> SectionSpecs a
sectionSpec = MkSectionSpecs . liftAp


runSections :: Applicative f => (forall x. SectionSpec x -> f x) -> SectionSpecs a -> f a
runSections f (MkSectionSpecs s) = runAp f s


runSections_ :: Monoid m => (forall x. SectionSpec x -> m) -> SectionSpecs a -> m
runSections_ f (MkSectionSpecs s) = runAp_ f s


------------------------------------------------------------------------
-- 'SectionSpecs' builders
------------------------------------------------------------------------


reqSection ::
  Spec a =>
  Text {- ^ section name -} ->
  Text {- ^ description  -} ->
  SectionSpecs a
reqSection n i = sectionSpec (ReqSection n i valuesSpec)


reqSection' ::
  Text         {- ^ section name  -} ->
  Text         {- ^ description   -} ->
  ValueSpecs a {- ^ value matcher -} ->
  SectionSpecs a
reqSection' n i w = sectionSpec (ReqSection n i w)


optSection ::
  Spec a =>
  Text {- ^ section name -} ->
  Text {- ^ description  -} ->
  SectionSpecs (Maybe a)
optSection n i = sectionSpec (OptSection n i valuesSpec)


optSection' ::
  Text         {- ^ section name  -} ->
  Text         {- ^ description   -} ->
  ValueSpecs a {- ^ value matcher -} ->
  SectionSpecs (Maybe a)
optSection' n i w = sectionSpec (OptSection n i w)


------------------------------------------------------------------------
-- Specifications for values
------------------------------------------------------------------------

data ValueSpec :: * -> * where
  -- | Matches any string literal
  TextSpec :: ValueSpec Text

  -- | Matches integral numbers
  IntegerSpec  :: ValueSpec Integer

  -- | Matches any number
  RationalSpec :: ValueSpec Rational

  -- | Matches any atom
  AnyAtomSpec  :: ValueSpec Text

  -- | Specific atom to be matched
  AtomSpec :: Text -> ValueSpec ()

  -- | Matches a list of the underlying specification
  ListSpec :: ValueSpecs a -> ValueSpec [a]

  -- | Documentation identifier and section specification
  SectionSpecs :: Text -> SectionSpecs a -> ValueSpec a

  -- | Documentation text, underlying specification, and filtering function
  CustomSpec :: Text -> ValueSpecs a -> (a -> Maybe b) -> ValueSpec b

  -- | Label used to hide complicated specs in documentation
  NamedSpec :: Text -> ValueSpecs a -> ValueSpec a


newtype ValueSpecs a = MkValueSpecs { unValueSpec :: Compose [] (Ap ValueSpec) a }
  deriving (Functor, Applicative, Alternative)


runValueSpecs :: Applicative f => (forall x. ValueSpec x -> f x) -> ValueSpecs a -> [f a]
runValueSpecs f =  map (runAp f) . getCompose . unValueSpec

runValueSpecs_ :: Monoid m => (forall x. ValueSpec x -> m) -> ValueSpecs a -> [m]
runValueSpecs_ f = map (runAp_ f) . getCompose . unValueSpec


valueSpec :: ValueSpec a -> ValueSpecs a
valueSpec = MkValueSpecs . Compose . pure . liftAp


------------------------------------------------------------------------
-- 'ValueSpecs' builders
------------------------------------------------------------------------


-- | Class for things that are easy to want
class    Spec a       where valuesSpec :: ValueSpecs a
instance Spec Text    where valuesSpec = valueSpec TextSpec
instance Spec Integer where valuesSpec = valueSpec IntegerSpec
instance Spec Int     where valuesSpec = fromInteger <$> valuesSpec
instance Spec Rational where valuesSpec = valueSpec RationalSpec
instance Spec a => Spec [a] where valuesSpec = valueSpec (ListSpec valuesSpec)
instance (Spec a, Spec b) => Spec (Either a b) where valuesSpec = valuesSpec <|> valuesSpec


atomSpec :: Text -> ValueSpecs ()
atomSpec = valueSpec . AtomSpec

anyAtomSpec :: ValueSpecs Text
anyAtomSpec = valueSpec AnyAtomSpec

stringSpec :: ValueSpecs String
stringSpec = Text.unpack <$> valuesSpec

numSpec :: Num a => ValueSpecs a
numSpec = fromInteger <$> valuesSpec

listSpec :: ValueSpecs a -> ValueSpecs [a]
listSpec = valueSpec . ListSpec


-- | Named subsection value specification. The unique identifier will be used
-- for generating a documentation section for this specification and will be.
sectionsSpec ::
  Text           {- ^ unique documentation identifier -} ->
  SectionSpecs a {- ^ underlying specification        -} ->
  ValueSpecs a
sectionsSpec i s = valueSpec (SectionSpecs i s)


namedSpec :: Text -> ValueSpecs a -> ValueSpecs a
namedSpec n s = valueSpec (NamedSpec n s)


oneOrList :: ValueSpecs a -> ValueSpecs [a]
oneOrList s = (:[]) <$> s <|> listSpec s


customSpec :: Text -> ValueSpecs a -> (a -> Maybe b) -> ValueSpecs b
customSpec lbl w f = valueSpec (CustomSpec lbl w f)
