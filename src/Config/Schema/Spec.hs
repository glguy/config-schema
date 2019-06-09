{-# Language FlexibleInstances, RankNTypes, GADTs, KindSignatures #-}
{-# Language DeriveFunctor, GeneralizedNewtypeDeriving, OverloadedStrings #-}

{-|
Module      : Config.Schema.Spec
Description : Operations for describing a configuration file format.
Copyright   : (c) Eric Mertens, 2017
License     : ISC
Maintainer  : emertens@gmail.com

This module provides a set of types and operations for defining configuration
file schemas.

These specifications are can be consumed by "Config.Schema.Load"
and "Config.Schema.Docs".

This is the schema system used by the @glirc@ IRC client
<https://hackage.haskell.org/package/glirc>. For a significant example,
visit the "Client.Configuration" and "Client.Configuration.Colors" modules.

-}
module Config.Schema.Spec
  (
  -- * Specifying values
  -- $values
    ValueSpec
  , sectionsSpec
  , assocSpec
  , atomSpec
  , anyAtomSpec
  , listSpec
  , customSpec
  , namedSpec
  , HasSpec(..)

  -- * Specifying sections
  -- $sections
  , SectionsSpec
  , reqSection
  , optSection
  , reqSection'
  , optSection'

  -- * Derived specifications
  , oneOrList
  , yesOrNoSpec
  , stringSpec
  , numSpec
  , fractionalSpec
  , nonemptySpec
  , oneOrNonemptySpec

  ) where

import           Data.Bits                        (Bits, toIntegralSized)
import           Data.Functor.Alt                 (Alt(..))
import           Data.Int
import           Data.List.NonEmpty               (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Text                        (Text)
import qualified Data.Text as Text
import           Data.Word

import           Config.Schema.Types

------------------------------------------------------------------------
-- 'ValueSpec' builders
------------------------------------------------------------------------

-- $values
--
-- 'ValueSpec' allows you to define specifications that will match
-- parsed config-value configuration files. 'ValueSpec' allows
-- us to define the shape of configuration values that will match
-- the specification as well as a way to process those matches.
--
-- Below we have an example configuration record that can be matched
-- from a configuration file.
--
-- More documentation for defining key-value pairs is available below.
--
-- This configuration file expects either a given username or allows
-- the user to ask for a random username. The ('<!>') operator allows
-- us to combine two alternatives as seen below. The config-value
-- language distinguishes between atoms like @random@ and strings like
-- @"random"@ allowing unambiguous special cases to be added in addition
-- to free-form text.
--
-- @
-- {-\# Language RecordWildCards, OverloadedStrings, ApplicativeDo \#-}
-- module Example where
--
-- import "Config.Schema"
-- import "Data.Functor.Alt" (('<!>'))
-- import "Data.Maybe"       ('Data.Maybe.fromMaybe')
-- import "Data.Text"        ('Text')
--
-- data Config = Config
--   { userName :: UserName
--   , retries  :: 'Int'
--   }
--
-- data UserName = Random | Given 'Text'
--
-- userNameSpec :: ValueSpec UserName
-- userNameSpec = Random '<$'  'atomSpec' \"random\"
--            '<!>' Given  '<$>' 'anySpec' -- matches string literals
--
-- nameExample :: 'ValueSpec' Config
-- nameExample = 'sectionsSpec' \"config\" '$'
--
--   do userName <- 'reqSection'' \"username\" userNameSpec \"Configured user name\"
--
--      retries  <- 'Data.Maybe.fromMaybe' 3
--              '<$>' 'optSection' \"retries\" \"Number of attempts (default: 3)\"
--
--      'pure' Config{..}
-- @
--
-- Examples:
--
-- > username: random
-- > retries: 5
-- > -- Generates: Config { userName = Random, retries = 5 }
--
-- We can omit the retries:
--
-- > username: random
-- > -- Generates: Config { userName = Random, retries = 3 }
--
-- We can specify a specific username as a string literal instead
-- of using the atom @random@:
--
-- > username: "me"
-- > -- Generates: Config { userName = Given "me", retries = 3 }
--
-- Sections can be reordered:
--
-- > retries: 5
-- > username: random
-- > -- Generates: Config { userName = Random, retries = 5 }

-- | Class of value specifications that don't require arguments.
class    HasSpec a        where anySpec :: ValueSpec a
instance HasSpec Text     where anySpec = primValueSpec TextSpec
instance HasSpec Integer  where anySpec = primValueSpec IntegerSpec
instance HasSpec Rational where anySpec = primValueSpec RationalSpec
instance HasSpec Int      where anySpec = sizedBitsSpec "machine-bit signed"
instance HasSpec Int8     where anySpec = sizedBitsSpec "8-bit signed"
instance HasSpec Int16    where anySpec = sizedBitsSpec "16-bit signed"
instance HasSpec Int32    where anySpec = sizedBitsSpec "32-bit signed"
instance HasSpec Int64    where anySpec = sizedBitsSpec "64-bit signed"
instance HasSpec Word     where anySpec = sizedBitsSpec "machine-bit unsigned"
instance HasSpec Word8    where anySpec = sizedBitsSpec "8-bit unsigned"
instance HasSpec Word16   where anySpec = sizedBitsSpec "16-bit unsigned"
instance HasSpec Word32   where anySpec = sizedBitsSpec "32-bit unsigned"
instance HasSpec Word64   where anySpec = sizedBitsSpec "64-bit unsigned"

instance HasSpec a => HasSpec [a] where
  anySpec = primValueSpec (ListSpec anySpec)

instance (HasSpec a, HasSpec b) => HasSpec (Either a b) where
  anySpec = Left <$> anySpec <!> Right <$> anySpec

sizedBitsSpec :: (Integral a, Bits a) => Text -> ValueSpec a
sizedBitsSpec name = customSpec name (primValueSpec IntegerSpec) check
  where
    check i = case toIntegralSized i of
                Nothing -> Left "out of bounds"
                Just j  -> Right j

-- | Specification for matching a particular atom.
atomSpec :: Text -> ValueSpec ()
atomSpec = primValueSpec . AtomSpec

-- | Specification for matching any atom. Matched atom is returned.
anyAtomSpec :: ValueSpec Text
anyAtomSpec = primValueSpec AnyAtomSpec

-- | Specification for matching any text as a 'String'
stringSpec :: ValueSpec String
stringSpec = Text.unpack <$> anySpec

-- | Specification for matching any integral number.
numSpec :: Num a => ValueSpec a
numSpec = fromInteger <$> anySpec

-- | Specification for matching any fractional number.
--
-- @since 0.2.0.0
fractionalSpec :: Fractional a => ValueSpec a
fractionalSpec = fromRational <$> anySpec

-- | Specification for matching a list of values each satisfying a
-- given element specification.
listSpec :: ValueSpec a -> ValueSpec [a]
listSpec = primValueSpec . ListSpec


-- | Named subsection value specification. The unique identifier will be used
-- for generating a documentation section for this specification and should
-- be unique within the scope of the specification being built.
sectionsSpec ::
  Text           {- ^ unique documentation identifier -} ->
  SectionsSpec a {- ^ underlying specification        -} ->
  ValueSpec a
sectionsSpec i s = primValueSpec (SectionsSpec i s)


-- | Specification for a section list where the keys are user-defined.
-- Values are matched against the underlying specification and returned
-- as a list of section-name\/value pairs.
--
-- @since 0.3.0.0
assocSpec ::
  ValueSpec a {- ^ underlying specification -} ->
  ValueSpec [(Text,a)]
assocSpec = primValueSpec . AssocSpec


-- | Named value specification. This is useful for factoring complicated
-- value specifications out in the documentation to avoid repetition of
-- complex specifications.
namedSpec ::
  Text         {- ^ name                     -} ->
  ValueSpec a {- ^ underlying specification -} ->
  ValueSpec a
namedSpec n s = primValueSpec (NamedSpec n s)


-- | Specification that matches either a single element or multiple
-- elements in a list. This can be convenient for allowing the user
-- to avoid having to specify singleton lists in the configuration file.
oneOrList :: ValueSpec a -> ValueSpec [a]
oneOrList s = pure <$> s <!> listSpec s


-- | The custom specification allows an arbitrary function to be used
-- to validate the value extracted by a specification. If 'Nothing'
-- is returned the value is considered to have failed validation.
customSpec :: Text -> ValueSpec a -> (a -> Either Text b) -> ValueSpec b
customSpec lbl w f = primValueSpec (CustomSpec lbl (f <$> w))


-- | Specification for using @yes@ and @no@ to represent booleans 'True'
-- and 'False' respectively
yesOrNoSpec :: ValueSpec Bool
yesOrNoSpec = True  <$ atomSpec (Text.pack "yes")
          <!> False <$ atomSpec (Text.pack "no")


-- | Matches a non-empty list.
--
-- @since 0.2.0.0
nonemptySpec :: ValueSpec a -> ValueSpec (NonEmpty a)
nonemptySpec s = customSpec "nonempty" (listSpec s) check
  where
    check xs = case NonEmpty.nonEmpty xs of
                 Nothing -> Left "empty list"
                 Just xxs -> Right xxs

-- | Matches a single element or a non-empty list.
--
-- @since 0.2.0.0
oneOrNonemptySpec :: ValueSpec a -> ValueSpec (NonEmpty a)
oneOrNonemptySpec s = pure <$> s <!> nonemptySpec s


------------------------------------------------------------------------
-- 'SectionsSpec' builders
------------------------------------------------------------------------

-- $sections
-- Sections specifications allow you to define an unordered collection
-- of required and optional sections using a convenient 'Applicative'
-- do-notation syntax.
--
-- Let's consider an example of a way to specify a name given a base
-- and optional suffix.
--
-- @
-- {-\# Language OverloadedStrings, ApplicativeDo \#-}
-- module Example where
--
-- import "Config.Schema"
-- import "Data.Text" ('Text')
--
-- nameExample :: 'ValueSpec' 'Text'
-- nameExample =
--   'sectionsSpec' \"name\" '$'
--   do x <- 'reqSection' \"base\" \"Base name\"
--      y <- 'optSection' \"suffix\" \"Optional name suffix\"
--      'pure' ('maybe' x (x '<>') y)
-- @
--
-- Example configuration components and their extracted values.
--
-- > base:     "VAR"
-- > optional: "1"
-- > -- Generates: VAR1
--
-- Order doesn't matter
--
-- > optional: "1"
-- > base:     "VAR"
-- > -- Generates: VAR1
--
-- Optional fields can be omitted
--
-- > base:     "VAR"
-- > -- Generates: VAR
--
-- Unexpected sections will generate errors to help detect typos
--
-- > base:     "VAR"
-- > extra:    0
-- > -- Failure due to unexpected extra section
--
-- All required sections must appear for successful match
--
-- > optional: "1"
-- > -- Failure due to missing required section

-- | Specification for a required section with an implicit value specification.
reqSection ::
  HasSpec a =>
  Text {- ^ section name -} ->
  Text {- ^ description  -} ->
  SectionsSpec a
reqSection n = reqSection' n anySpec


-- | Specification for a required section with an explicit value specification.
reqSection' ::
  Text        {- ^ section name        -} ->
  ValueSpec a {- ^ value specification -} ->
  Text        {- ^ description         -} ->
  SectionsSpec a
reqSection' n w i = primSectionsSpec (ReqSection n i w)


-- | Specification for an optional section with an implicit value specification.
optSection ::
  HasSpec a =>
  Text {- ^ section name -} ->
  Text {- ^ description  -} ->
  SectionsSpec (Maybe a)
optSection n = optSection' n anySpec


-- | Specification for an optional section with an explicit value specification.
optSection' ::
  Text        {- ^ section name        -} ->
  ValueSpec a {- ^ value specification -} ->
  Text        {- ^ description         -} ->
  SectionsSpec (Maybe a)
optSection' n w i = primSectionsSpec (OptSection n i w)
