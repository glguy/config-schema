{-# Language FlexibleInstances, RankNTypes, GADTs, KindSignatures #-}
{-# Language GeneralizedNewtypeDeriving #-}

{-|
Module      : Config.Schema.Spec
Description : Types and operations for describing a configuration file format.
Copyright   : (c) Eric Mertens, 2017
License     : ISC
Maintainer  : emertens@gmail.com

-}
module Config.Schema.Spec where

import           Control.Applicative.Free         (Ap, runAp, runAp_, liftAp)
import           Data.Functor.Const               (Const(..))
import           Data.Functor.Coyoneda            (Coyoneda(..), liftCoyoneda, lowerCoyoneda)
import           Data.Functor.Compose             (Compose(..), getCompose)
import           Data.Functor.Alt                 (Alt(..))
import           Data.List.NonEmpty               (NonEmpty)
import           Data.Text                        (Text)
import qualified Data.Text as Text

------------------------------------------------------------------------
-- Specifications for sections
------------------------------------------------------------------------

-- | Specifications for single configuration sections.
--
-- The fields are section name, documentation text, value specification.
-- Use 'ReqSection' for required key-value pairs and 'OptSection' for
-- optional ones.
data SectionSpec :: * -> * where
  ReqSection :: Text -> Text -> ValueSpecs a -> SectionSpec a
  OptSection :: Text -> Text -> ValueSpecs a -> SectionSpec (Maybe a)


-- | A list of section specifiations used to process a whole group of
-- key-value pairs. Multiple section specifications can be combined
-- using this type's 'Applicative' instance.
newtype SectionSpecs a = MkSectionSpecs (Ap SectionSpec a)
  deriving (Functor, Applicative)


-- | Lift a single specification into a list of specifications.
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

  -- | Documentation text, underlying specification
  CustomSpec :: Text -> ValueSpecs (Maybe a) -> ValueSpec a

  -- | Label used to hide complicated specs in documentation
  NamedSpec :: Text -> ValueSpecs a -> ValueSpec a


newtype ValueSpecs a = MkValueSpecs { unValueSpecs :: Compose NonEmpty (Coyoneda ValueSpec) a }
  deriving Functor

instance Alt ValueSpecs where MkValueSpecs x <!> MkValueSpecs y = MkValueSpecs (x <!> y)


runValueSpecs :: Functor f => (forall x. ValueSpec x -> f x) -> ValueSpecs a -> NonEmpty (f a)
runValueSpecs f =  fmap (lowerCoyoneda . hoistCoyoneda f) . getCompose . unValueSpecs


runValueSpecs_ :: (forall x. ValueSpec x -> m) -> ValueSpecs a -> NonEmpty m
runValueSpecs_ f = fmap getConst . runValueSpecs (Const . f)


valueSpec :: ValueSpec a -> ValueSpecs a
valueSpec = MkValueSpecs . Compose . pure . liftCoyoneda


------------------------------------------------------------------------
-- 'ValueSpecs' builders
------------------------------------------------------------------------


-- | Class for things that are easy to want
class    Spec a       where valuesSpec :: ValueSpecs a
instance Spec Text    where valuesSpec = valueSpec TextSpec
instance Spec Integer where valuesSpec = valueSpec IntegerSpec
instance Spec Rational where valuesSpec = valueSpec RationalSpec
instance Spec Int     where valuesSpec = fromInteger <$> valuesSpec
instance Spec a => Spec [a] where valuesSpec = valueSpec (ListSpec valuesSpec)
instance (Spec a, Spec b) => Spec (Either a b) where
  valuesSpec = Left <$> valuesSpec <!> Right <$> valuesSpec


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
oneOrList s = pure <$> s <!> listSpec s


customSpec :: Text -> ValueSpecs a -> (a -> Maybe b) -> ValueSpecs b
customSpec lbl w f = valueSpec (CustomSpec lbl (f <$> w))

------------------------------------------------------------------------

hoistCoyoneda :: (forall x. f x -> g x) -> Coyoneda f a -> Coyoneda g a
hoistCoyoneda f (Coyoneda g x) = Coyoneda g (f x)