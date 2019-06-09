{-# Language StandaloneDeriving, CPP, KindSignatures, RankNTypes, GADTs, DeriveTraversable, GeneralizedNewtypeDeriving #-}
{-|
Module      : Config.Schema.Types
Description : Types for describing a configuration file format.
Copyright   : (c) Eric Mertens, 2017
License     : ISC
Maintainer  : emertens@gmail.com

This module defines the syntax of value specifications.

Specifications can be defined using "Config.Schema.Spec" and can be consumed
with "Config.Schema.Load" and "Config.Schema.Doc".

This module defines high-level 'ValueSpec' and 'SectionsSpec' types that are
intended to be used by normal library users. This types are implemented in
terms of primitive 'PrimValueSpec' and 'PrimSectionSpec' types. These
primitives are what consumers of specifications will need to use.

-}
module Config.Schema.Types
  (
  -- * Value specification
    ValueSpec
  , PrimValueSpec(..)
  , primValueSpec
  , runValueSpec
  , runValueSpec_

  -- * Unordered section-value pairs specification
  , SectionsSpec
  , PrimSectionSpec(..)
  , primSectionsSpec
  , runSections
  , runSections_

  ) where

import           Control.Applicative      (Const(..))
import           Control.Applicative.Free (Ap, liftAp, runAp, runAp_)
import           Data.Functor.Alt         (Alt(..))
import           Data.Functor.Coyoneda    (Coyoneda(..), liftCoyoneda, lowerCoyoneda, hoistCoyoneda)
import           Data.List.NonEmpty       (NonEmpty)
import           Data.Semigroup.Foldable  (asum1, foldMap1)
import           Data.Text                (Text)

#if !MIN_VERSION_base(4,11,0)
import           Data.Semigroup           (Semigroup)
#endif

------------------------------------------------------------------------
-- Specifications for values
------------------------------------------------------------------------

-- | The primitive specification descriptions for values. Specifications
-- built from these primitive cases are found in 'ValueSpec'.
data PrimValueSpec :: * -> * where
  -- | Matches any string literal
  TextSpec :: PrimValueSpec Text

  -- | Matches integral numbers
  IntegerSpec :: PrimValueSpec Integer

  -- | Matches any number
  RationalSpec :: PrimValueSpec Rational

  -- | Matches any atom
  AnyAtomSpec :: PrimValueSpec Text

  -- | Specific atom to be matched
  AtomSpec :: Text -> PrimValueSpec ()

  -- | Matches a list of the underlying specification
  ListSpec :: ValueSpec a -> PrimValueSpec [a]

  -- | Documentation identifier and sections specification
  SectionsSpec :: Text -> SectionsSpec a -> PrimValueSpec a

  -- | Matches an arbitrary list of sections. Similar to 'SectionsSpec'
  -- except that that the section names are user-defined.
  AssocSpec :: ValueSpec a -> PrimValueSpec [(Text,a)]

  -- | Documentation text and underlying specification. This specification
  -- will match values where the underlying specification returns a
  -- 'Right' value. Otherwise a 'Left' should contain a short failure 
  -- explanation.
  CustomSpec :: Text -> ValueSpec (Either Text a) -> PrimValueSpec a

  -- | Label used to hide complex specifications in documentation.
  NamedSpec :: Text -> ValueSpec a -> PrimValueSpec a

-- | Non-empty disjunction of value specifications. This type is the primary
-- way to specify expected values.
--
-- Multiple specifications can be combined using this type's 'Alt' instance.
newtype ValueSpec a = MkValueSpec
  { unValueSpec :: NonEmpty (Coyoneda PrimValueSpec a) }
  deriving (Functor)

-- | Lift a primitive value specification to 'ValueSpec'.
--
-- @since 0.2.0.0
primValueSpec :: PrimValueSpec a -> ValueSpec a
primValueSpec = MkValueSpec . pure . liftCoyoneda

-- | Given an interpretation of a primitive value specification, extract a list of
-- the possible interpretations of a disjunction of value specifications. Each of
-- these primitive interpretations will be combined using the provided 'Alt' instance.
runValueSpec :: Alt f => (forall x. PrimValueSpec x -> f x) -> ValueSpec a -> f a
runValueSpec f = asum1 . fmap (runCoyoneda f) . unValueSpec


-- | Given an interpretation of a primitive value specification, extract a list of
-- the possible interpretations of a disjunction of value specifications. Each of
-- these primitive interpretations will be combined using the provided 'Semigroup' instance.
runValueSpec_ :: Semigroup m => (forall x. PrimValueSpec x -> m) -> ValueSpec a -> m
runValueSpec_ f = foldMap1 (runCoyoneda_ f) . unValueSpec


-- Helper for transforming the underlying type @f@ to one supporting a 'Functor'
-- instance before lowering.
runCoyoneda :: Functor g => (forall a. f a -> g a) -> Coyoneda f b -> g b
runCoyoneda f = lowerCoyoneda . hoistCoyoneda f

-- Helper for extracting the the value stored in a 'Coyoneda' while forgetting its
-- type index.
runCoyoneda_ :: (forall a. f a -> m) -> Coyoneda f b -> m
runCoyoneda_ f = getConst . runCoyoneda (Const . f)

-- | Left-biased choice between two specifications
instance Alt ValueSpec where MkValueSpec x <!> MkValueSpec y = MkValueSpec (x <!> y)

------------------------------------------------------------------------
-- Specifications for sections
------------------------------------------------------------------------

-- | Specifications for single configuration sections.
--
-- The fields are section name, documentation text, value specification.
-- Use 'ReqSection' for required key-value pairs and 'OptSection' for
-- optional ones.
data PrimSectionSpec :: * -> * where

  -- | Required section: Name, Documentation, Specification
  ReqSection :: Text -> Text -> ValueSpec a -> PrimSectionSpec a

  -- | Optional section: Name, Documentation, Specification
  OptSection :: Text -> Text -> ValueSpec a -> PrimSectionSpec (Maybe a)


-- | A list of section specifications used to process a whole group of
-- key-value pairs. Multiple section specifications can be combined
-- using this type's 'Applicative' instance.
newtype SectionsSpec a = MkSectionsSpec (Ap PrimSectionSpec a)
  deriving (Functor, Applicative)


-- | Lift a single specification into a list of specifications.
--
-- @since 0.2.0.0
primSectionsSpec :: PrimSectionSpec a -> SectionsSpec a
primSectionsSpec = MkSectionsSpec . liftAp

-- | Given an function that handles a single, primitive section specification;
-- 'runSections' will generate one that processes a whole 'SectionsSpec'.
--
-- The results from each section will be sequence together using the 'Applicative'
-- instance in of the result type, and the results can be indexed by the type
-- parameter of the specification.
--
-- For an example use of 'runSections', see "Config.Schema.Load".
runSections :: Applicative f => (forall x. PrimSectionSpec x -> f x) -> SectionsSpec a -> f a
runSections f (MkSectionsSpec s) = runAp f s


-- | Given an function that handles a single, primitive section specification;
-- 'runSections_' will generate one that processes a whole 'SectionsSpec'.
--
-- The results from each section will be sequence together using the 'Monoid'
-- instance in of the result type, and the results will not be indexed by the
-- type parameter of the specifications.
--
-- For an example use of 'runSections_', see "Config.Schema.Docs".
runSections_ :: Monoid m => (forall x. PrimSectionSpec x -> m) -> SectionsSpec a -> m
runSections_ f (MkSectionsSpec s) = runAp_ f s
