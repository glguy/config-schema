{-# Language ScopedTypeVariables, OverloadedStrings #-}

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
visit the @Client.Configuration@ and @Client.Configuration.Colors@ modules.

-}
module Config.Schema.Spec
  (
  -- * Specifying values
  -- $values

    ValueSpec
  , customSpec
  , namedSpec
  , exactSpec
  , HasSpec(..)

  -- ** Key-value mapping specifications
  -- $sections
  , sectionsSpec
  , assocSpec

  -- ** Number specifications
  -- $number
  , numberSpec
  , integerSpec
  , rationalSpec
  , naturalSpec
  , fractionalSpec
  , numSpec

  -- ** Text specifications
  -- $text
  , textSpec
  , stringSpec

  -- ** Atom specifications
  -- $atom
  , atomSpec
  , anyAtomSpec
  , yesOrNoSpec
  , trueOrFalseSpec

  -- ** List specifications
  -- $list
  , listSpec
  , oneOrList
  , nonemptySpec
  , oneOrNonemptySpec


  -- * Specifying sections
  -- $sectionsspec
  , SectionsSpec
  , reqSection
  , optSection
  , reqSection'
  , optSection'

  -- * Re-exports
  , Alt(..)

  ) where

import           Data.Bits                        (FiniteBits, isSigned, toIntegralSized, finiteBitSize)
import           Data.Functor.Alt                 (Alt(..))
import           Data.Int
import           Data.List.NonEmpty               (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Text                        (Text)
import qualified Data.Text as Text
import           Data.Word
import           Data.Ratio
import           GHC.Natural                      (Natural)

import           Config.Schema.Types
import           Config (Value(..), Atom(..))
import           Config.Number (Number, numberToInteger, numberToRational)

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

-- | Class of value specifications without parameters.
class    HasSpec a        where anySpec :: ValueSpec a
instance HasSpec Text     where anySpec = textSpec
instance HasSpec Integer  where anySpec = integerSpec
instance HasSpec Int      where anySpec = sizedBitsSpec
instance HasSpec Int8     where anySpec = sizedBitsSpec
instance HasSpec Int16    where anySpec = sizedBitsSpec
instance HasSpec Int32    where anySpec = sizedBitsSpec
instance HasSpec Int64    where anySpec = sizedBitsSpec
instance HasSpec Word     where anySpec = sizedBitsSpec
instance HasSpec Word8    where anySpec = sizedBitsSpec
instance HasSpec Word16   where anySpec = sizedBitsSpec
instance HasSpec Word32   where anySpec = sizedBitsSpec
instance HasSpec Word64   where anySpec = sizedBitsSpec

-- | @since 1.2.0.0
instance HasSpec Natural  where anySpec = naturalSpec

-- | @since 1.2.0.0
instance HasSpec Double   where anySpec = fractionalSpec

-- | @since 1.2.0.0
instance HasSpec Float    where anySpec = fractionalSpec

-- | For 'Ratio' and 'Rational'
--
-- @since 1.2.0.0
instance Integral a => HasSpec (Ratio a) where
  anySpec = fractionalSpec

-- | Zero or more elements in a list
instance HasSpec a => HasSpec [a] where
  anySpec = listSpec anySpec

-- | One or more elements in a list
--
-- @since 1.2.0.0
instance HasSpec a => HasSpec (NonEmpty a) where
  anySpec = nonemptySpec anySpec

-- | Left-biased, untagged union of specs
instance (HasSpec a, HasSpec b) => HasSpec (Either a b) where
  anySpec = Left <$> anySpec <!> Right <$> anySpec

-- | Named value specification. This is useful for factoring complicated
-- value specifications out in the documentation to avoid repetition of
-- complex specifications.
namedSpec ::
  Text        {- ^ name                     -} ->
  ValueSpec a {- ^ underlying specification -} ->
  ValueSpec a
namedSpec n s = primValueSpec (NamedSpec n s)


-- | The custom specification allows an arbitrary function to be used
-- to validate the value extracted by a specification. If 'Nothing'
-- is returned the value is considered to have failed validation.
customSpec :: Text -> ValueSpec a -> (a -> Either Text b) -> ValueSpec b
customSpec lbl w f = primValueSpec (CustomSpec lbl (f <$> w))

-- | Match an exact value. This can be used to match a specific text
-- literal number literal, atom, list of exact things, etc.
exactSpec :: Value () -> ValueSpec ()
exactSpec = primValueSpec . ExactSpec

------------------------------------------------------------------------

-- $sections
-- Specifications that match key-value map literals.

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

------------------------------------------------------------------------

-- $number
-- Specifications built from 'numberSpec' matching number literals.

-- | Primitive specification for matching any number.
--
-- @since 1.2.0.0
numberSpec :: ValueSpec Number
numberSpec = primValueSpec NumberSpec

{-# INLINE sizedBitsSpec #-}
sizedBitsSpec :: forall a. (Integral a, FiniteBits a) => ValueSpec a
sizedBitsSpec = customSpec label integerSpec check
  where
    signText = if isSigned (0::a) then "signed" else "unsigned"

    label = Text.pack (show (finiteBitSize (0::a)) ++ "-bit " ++ signText)

    check i = case toIntegralSized i of
                Nothing -> Left "out of bounds"
                Just j  -> Right j

-- | Specification for matching any non-negative, integral number
--
-- @since 1.2.0.0
naturalSpec :: ValueSpec Natural
naturalSpec = customSpec "non-negative" integerSpec check
  where
    check i
      | i < 0     = Left "negative number"
      | otherwise = Right (fromInteger i)

-- | Specification for matching any integral number.
numSpec :: Num a => ValueSpec a
numSpec = fromInteger <$> integerSpec

-- | Specification for matching any fractional number.
--
-- @since 0.2.0.0
fractionalSpec :: Fractional a => ValueSpec a
fractionalSpec = fromRational <$> rationalSpec

-- | Specification for matching any integral number.
--
-- @since 1.2.0.0
integerSpec :: ValueSpec Integer
integerSpec = customSpec "integral" numberSpec check
  where
    check n =
      case numberToInteger n of
        Nothing -> Left "fractional number"
        Just i  -> Right i

-- | Specification for matching any number as a 'Rational'.
--
-- @since 1.2.0.0
rationalSpec :: ValueSpec Rational
rationalSpec = numberToRational <$> numberSpec

------------------------------------------------------------------------

-- $atom
-- Specifications built to match atoms.

-- | Primitive specification for matching a particular atom.
atomSpec ::
  Text {- ^ atom -} ->
  ValueSpec ()
atomSpec t = () <$ primValueSpec (ExactSpec (Atom () (MkAtom t)))

-- | Primitive specification for matching any atom. Matched atom is returned.
anyAtomSpec :: ValueSpec Text
anyAtomSpec = primValueSpec AtomSpec

-- | Specification for using atoms @yes@ and @no@ to represent booleans 'True'
-- and 'False' respectively
yesOrNoSpec :: ValueSpec Bool
yesOrNoSpec = True <$ atomSpec "yes" <!> False <$ atomSpec "no"

-- | Specification for using atoms @true@ and @false@ to represent booleans 'True'
-- and 'False' respectively.
--
-- @since 1.2.0.0
trueOrFalseSpec :: ValueSpec Bool
trueOrFalseSpec = True <$ atomSpec "true" <!> False <$ atomSpec "false"

------------------------------------------------------------------------

-- $text
-- Specifications built from 'textSpec' for matching string literals.

-- | Specification for matching any text literal
--
-- @since 1.2.0.0
textSpec :: ValueSpec Text
textSpec = primValueSpec TextSpec

-- | Specification for matching any text as a 'String'
stringSpec :: ValueSpec String
stringSpec = Text.unpack <$> textSpec

------------------------------------------------------------------------

-- $list
-- Specifications for matching list literals built with 'listSpec.

-- | Primitive specification for matching a list of values each satisfying a
-- given element specification.
listSpec ::
  ValueSpec a {- ^ element specification -} ->
  ValueSpec [a]
listSpec = primValueSpec . ListSpec

-- | Matches a non-empty list.
--
-- @since 0.2.0.0
nonemptySpec ::
  ValueSpec a {- ^ element specification -} ->
  ValueSpec (NonEmpty a)
nonemptySpec s = customSpec "nonempty" (listSpec s) check
  where
    check xs = case NonEmpty.nonEmpty xs of
                 Nothing -> Left "empty list"
                 Just xxs -> Right xxs

-- | Matches a single element or a non-empty list.
--
-- @since 0.2.0.0
oneOrNonemptySpec ::
  ValueSpec a {- ^ element specification -} ->
  ValueSpec (NonEmpty a)
oneOrNonemptySpec s = pure <$> s <!> nonemptySpec s

-- | Specification that matches either a single element or multiple
-- elements in a list. This can be convenient for allowing the user
-- to avoid having to specify singleton lists in the configuration file.
oneOrList ::
  ValueSpec a {- ^ element specification -} ->
  ValueSpec [a]
oneOrList s = pure <$> s <!> listSpec s

------------------------------------------------------------------------
-- 'SectionsSpec' builders
------------------------------------------------------------------------

-- $sectionsspec
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
