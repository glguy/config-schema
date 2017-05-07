{-# Language FlexibleInstances, RankNTypes, GADTs, KindSignatures #-}
{-# Language GeneralizedNewtypeDeriving #-}

{-|
Module      : Config.Schema.Spec
Description : Types and operations for describing a configuration file format.
Copyright   : (c) Eric Mertens, 2017
License     : ISC
Maintainer  : emertens@gmail.com

This module provides a set of types and operations for defining configuration
file schemas. These schemas can be built up using 'Applicative' operations.

These specifications are suitable for be consumed by "Config.Schema.Load"
and "Config.Schema.Docs".

This is the schema system used by the @glirc@ IRC client
<https://hackage.haskell.org/package/glirc>. For a significant example,
visit the "Client.Configuration" and "Client.Configuration.Colors" modules.

-}
module Config.Schema.Spec
  (
  -- * Specifying sections
    SectionSpecs(..)
  , reqSection
  , optSection
  , reqSection'
  , optSection'

  -- * Specifying values
  , ValueSpecs(..)
  , Spec(..)
  , sectionsSpec
  , atomSpec
  , anyAtomSpec
  , listSpec
  , customSpec
  , namedSpec

  -- * Derived specifications
  , oneOrList
  , numSpec
  , yesOrNoSpec
  , stringSpec

  -- * Executing specifications
  , runSections
  , runSections_
  , runValueSpecs
  , runValueSpecs_

  -- * Primitive specifications
  , SectionSpec(..)
  , ValueSpec(..)

  ) where

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

  -- | Required section: Name, Documentation, Specification
  ReqSection :: Text -> Text -> ValueSpecs a -> SectionSpec a

  -- | Optional section: Name, Documentation, Specification
  OptSection :: Text -> Text -> ValueSpecs a -> SectionSpec (Maybe a)


-- | A list of section specifications used to process a whole group of
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


-- | Specification for a required section with an implicit value specification.
reqSection ::
  Spec a =>
  Text {- ^ section name -} ->
  Text {- ^ description  -} ->
  SectionSpecs a
reqSection n i = sectionSpec (ReqSection n i valuesSpec)


-- | Specification for a required section with an explicit value specification.
reqSection' ::
  Text         {- ^ section name        -} ->
  Text         {- ^ description         -} ->
  ValueSpecs a {- ^ value specification -} ->
  SectionSpecs a
reqSection' n i w = sectionSpec (ReqSection n i w)


-- | Specification for an optional section with an implicit value specification.
optSection ::
  Spec a =>
  Text {- ^ section name -} ->
  Text {- ^ description  -} ->
  SectionSpecs (Maybe a)
optSection n i = sectionSpec (OptSection n i valuesSpec)


-- | Specification for an optional section with an explicit value specification.
optSection' ::
  Text         {- ^ section name        -} ->
  Text         {- ^ description         -} ->
  ValueSpecs a {- ^ value specification -} ->
  SectionSpecs (Maybe a)
optSection' n i w = sectionSpec (OptSection n i w)


------------------------------------------------------------------------
-- Specifications for values
------------------------------------------------------------------------

-- | The primitive specification descriptions for values. Specifications
-- built from these primitive cases are found in 'ValueSpecs'.
data ValueSpec :: * -> * where
  -- | Matches any string literal
  TextSpec :: ValueSpec Text

  -- | Matches integral numbers
  IntegerSpec :: ValueSpec Integer

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


-- | Non-empty disjunction of value specifications. This type is the primary
-- way to specify expected values. Use the 'Spec' class to generate 'ValueSpecs'
-- for simple types.
newtype ValueSpecs a = MkValueSpecs { unValueSpecs :: Compose NonEmpty (Coyoneda ValueSpec) a }
  deriving Functor

-- | Left-biased choice between two specifications
instance Alt ValueSpecs where MkValueSpecs x <!> MkValueSpecs y = MkValueSpecs (x <!> y)


-- | Given an interpretation of a primitive value specification, extract a list of
-- the possible interpretations of a disjunction of value specifications.
--
-- Unlike 'runValueSpecs_', this allows the result of the interpretation to be indexed
-- by the type of the primitive value specifications.
runValueSpecs :: Functor f => (forall x. ValueSpec x -> f x) -> ValueSpecs a -> NonEmpty (f a)
runValueSpecs f =  fmap (lowerCoyoneda . hoistCoyoneda f) . getCompose . unValueSpecs


-- | Given an interpretation of a primitive value specification, extract a list of
-- the possible interpretations of a disjunction of value specifications.
runValueSpecs_ :: (forall x. ValueSpec x -> m) -> ValueSpecs a -> NonEmpty m
runValueSpecs_ f = fmap getConst . runValueSpecs (Const . f)


-- | Lift a primitive value specification to 'ValueSpecs'.
valueSpec :: ValueSpec a -> ValueSpecs a
valueSpec = MkValueSpecs . Compose . pure . liftCoyoneda


------------------------------------------------------------------------
-- 'ValueSpecs' builders
------------------------------------------------------------------------


-- | Class of value specifications that don't require arguments.
class    Spec a       where valuesSpec :: ValueSpecs a
instance Spec Text    where valuesSpec = valueSpec TextSpec
instance Spec Integer where valuesSpec = valueSpec IntegerSpec
instance Spec Rational where valuesSpec = valueSpec RationalSpec
instance Spec Int     where valuesSpec = fromInteger <$> valuesSpec
instance Spec a => Spec [a] where valuesSpec = valueSpec (ListSpec valuesSpec)
instance (Spec a, Spec b) => Spec (Either a b) where
  valuesSpec = Left <$> valuesSpec <!> Right <$> valuesSpec


-- | Specification for matching a particular atom.
atomSpec :: Text -> ValueSpecs ()
atomSpec = valueSpec . AtomSpec

-- | Specification for matching any atom. Matched atom is returned.
anyAtomSpec :: ValueSpecs Text
anyAtomSpec = valueSpec AnyAtomSpec

-- | Specification for matching any text as a 'String'
stringSpec :: ValueSpecs String
stringSpec = Text.unpack <$> valuesSpec

-- | Specification for matching any integral number.
numSpec :: Num a => ValueSpecs a
numSpec = fromInteger <$> valuesSpec

-- | Specification for matching a list of values each satisfying a
-- given element specification.
listSpec :: ValueSpecs a -> ValueSpecs [a]
listSpec = valueSpec . ListSpec


-- | Named subsection value specification. The unique identifier will be used
-- for generating a documentation section for this specification and should
-- be unique within the scope of the specification being built.
sectionsSpec ::
  Text           {- ^ unique documentation identifier -} ->
  SectionSpecs a {- ^ underlying specification        -} ->
  ValueSpecs a
sectionsSpec i s = valueSpec (SectionSpecs i s)


-- | Named value specification. This is useful for factoring complicated
-- value specifications out in the documentation to avoid repetition of
-- complex specifications.
namedSpec ::
  Text         {- ^ name                     -} ->
  ValueSpecs a {- ^ underlying specification -} ->
  ValueSpecs a
namedSpec n s = valueSpec (NamedSpec n s)


-- | Specification that matches either a single element or multiple
-- elements in a list. This can be convenient for allowing the user
-- to avoid having to specify singleton lists in the configuration file.
oneOrList :: ValueSpecs a -> ValueSpecs [a]
oneOrList s = pure <$> s <!> listSpec s


-- | The custom specification allows an arbitrary function to be used
-- to validate the value extracted by a specification. If 'Nothing'
-- is returned the value is considered to have failed validation.
customSpec :: Text -> ValueSpecs a -> (a -> Maybe b) -> ValueSpecs b
customSpec lbl w f = valueSpec (CustomSpec lbl (f <$> w))


-- | Specification for using @yes@ and @no@ to represent booleans 'True'
-- and 'False' respectively
yesOrNoSpec :: ValueSpecs Bool
yesOrNoSpec = True  <$ atomSpec (Text.pack "yes")
          <!> False <$ atomSpec (Text.pack "no")

------------------------------------------------------------------------

-- | Lift a natural transformation to be a natural transformation of
-- 'Coyoneda'.
hoistCoyoneda :: (forall x. f x -> g x) -> (Coyoneda f a -> Coyoneda g a)
hoistCoyoneda f (Coyoneda g x) = Coyoneda g (f x)
