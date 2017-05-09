{-# Language OverloadedStrings, GADTs, GeneralizedNewtypeDeriving #-}

{-|
Module      : Config.Schema.Docs
Description : Documentation generation for config schemas
Copyright   : (c) Eric Mertens, 2017
License     : ISC
Maintainer  : emertens@gmail.com

This module generates a simple textual documentation format for a configuration
schema. Each subsection and named value specification will generate it's own
top-level component in the documentation.

This module is only one of the ways one could generate documentation for a
particular configuration specification. All of the defintions would would need
to be able to generate another form are exported by "Config.Schema.Spec".

@
configSpec :: ValueSpecs (Text,Maybe Int)
configSpec = sectionsSpec ""
           $ liftA2 (,)
               (reqSection "username" "Name used to login")
               (optSection "attempts" "Number of login attempts")

generateDocs configSpec

-- Configuration file fields:
--     username: REQUIRED text
--        Name used to login
--     attempts: integer
--        Number of login attempts
@

-}
module Config.Schema.Docs
  ( generateDocs
  ) where

import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as Text

import           Config.Schema.Spec

-- | Default documentation generator. This generator is specifically
-- for configuration specifications where the top-level specification
-- is named with the empty string (@""@).
generateDocs :: ValueSpecs a -> Text
generateDocs spec = Text.unlines
  ("Configuration file fields:"
   : map ("    " <>) top
  ++ concatMap sectionLines (Map.toList m'))

  where
    topname = ""
    Just top = Map.lookup topname m
    DocBuilder (m,"") = valuesDoc spec
    m' = Map.delete topname m

    sectionLines :: (Text, [Text]) -> [Text]
    sectionLines (name, fields)
      = ""
      : name
      : map ("    "<>) fields


-- | Compute the documentation for a list of sections, store the
-- documentation in the sections map and return the name of the section.
sectionsDoc :: Text -> SectionSpecs a -> DocBuilder Text
sectionsDoc l spec = emitDoc l =<< runSections_ sectionDoc spec


-- | Compute the documentation lines for a single key-value pair.
sectionDoc :: SectionSpec a -> DocBuilder [Text]
sectionDoc s =
  case s of
    ReqSection name desc w -> aux "REQUIRED " name desc <$> valuesDoc w
    OptSection name desc w -> aux ""          name desc <$> valuesDoc w
  where
    aux req name desc val =
      (name <> ": " <> req <> val)
      : if Text.null desc then [] else ["   " <> desc]


-- | Compute the documentation line for a particular value specification.
-- Any sections contained in the specification will be stored in the
-- sections map.
valuesDoc :: ValueSpecs a -> DocBuilder Text
valuesDoc = fmap disjunction . sequenceA . runValueSpecs_ (fmap pure valueDoc)


-- | Combine a list of text with the word @or@.
disjunction :: [Text] -> Text
disjunction = Text.intercalate " or "


-- | Compute the documentation fragment for an individual value specification.
valueDoc :: ValueSpec a -> DocBuilder Text
valueDoc w =
  case w of
    TextSpec         -> return "text"
    IntegerSpec      -> return "integer"
    RationalSpec     -> return "number"
    AtomSpec a       -> return ("`" <> a <> "`")
    AnyAtomSpec      -> return "atom"
    SectionSpecs l s -> sectionsDoc l s
    NamedSpec    l s -> emitDoc l . pure =<< valuesDoc s
    CustomSpec l w'  -> ((l <> " ") <>) <$> valuesDoc w'
    ListSpec ws      -> ("list of " <>) <$> valuesDoc ws
    AssocSpec ws     -> ("assocation list of " <>) <$> valuesDoc ws


-- | A writer-like type. A mapping of section names and documentation
-- lines is accumulated.
newtype DocBuilder a = DocBuilder (Map Text [Text], a)
  deriving (Functor, Applicative, Monad, Monoid, Show)


-- | Given a section name and section body, store the body
-- in the map of sections and return the section name.
emitDoc ::
  Text   {- ^ section name -} ->
  [Text] {- ^ section body -} ->
  DocBuilder Text
emitDoc l xs = DocBuilder (Map.singleton l xs, l)
