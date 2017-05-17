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

-- Top-level configuration file fields:
--     username: REQUIRED text
--        Name used to login
--     attempts: integer
--        Number of login attempts
@

-}
module Config.Schema.Docs
  ( generateDocs
  ) where

import           Data.List (intersperse)
import           Data.List.NonEmpty (NonEmpty((:|)))
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Text (Text)
import qualified Data.Text as Text
import           Text.PrettyPrint (Doc, fsep, text, ($+$), (<>), (<+>), nest, empty, hsep)

import           Config.Schema.Spec

-- | Default documentation generator.
generateDocs :: ValueSpecs a -> Doc
generateDocs spec = vcat' docLines
  where
    sectionLines :: (Text, Doc) -> [Doc]
    sectionLines (name, fields) = [text "", txt name, nest 4 fields]

    (topMap, topDoc) = runDocBuilder (valuesDoc spec)

    docLines =
      case runValueSpecs_ (pure . SomeSpec) spec of
        -- single, top-level sections spec
        SomeSpec (SectionSpecs name _) :| []
          | Just top <- Map.lookup name topMap ->
              txt "Top-level configuration file fields:" :
              nest 4 top :
              concatMap sectionLines (Map.toList (Map.delete name topMap))

        -- otherwise
        _ -> txt "Top-level configuration file format:" :
             nest 4 topDoc :
             concatMap sectionLines (Map.toList topMap)


-- | Forget the type of the value spec
data SomeSpec where SomeSpec :: ValueSpec a -> SomeSpec


-- | Compute the documentation for a list of sections, store the
-- documentation in the sections map and return the name of the section.
sectionsDoc :: Text -> SectionSpecs a -> DocBuilder Doc
sectionsDoc l spec = emitDoc l . vcat' =<< runSections_ (fmap pure . sectionDoc) spec


-- | Compute the documentation lines for a single key-value pair.
sectionDoc :: SectionSpec a -> DocBuilder Doc
sectionDoc s =
  case s of
    ReqSection name desc w -> aux "REQUIRED" name desc <$> valuesDoc w
    OptSection name desc w -> aux empty      name desc <$> valuesDoc w
  where
    aux req name desc val =
      txt name <> ":" <+> req <+> val $+$
      if Text.null desc
        then empty
        else nest 4 (fsep (txt <$> Text.splitOn " " desc)) -- line wrap logic


-- | Compute the documentation line for a particular value specification.
-- Any sections contained in the specification will be stored in the
-- sections map.
valuesDoc :: ValueSpecs a -> DocBuilder Doc
valuesDoc = fmap disjunction . sequenceA . runValueSpecs_ (fmap pure valueDoc)


-- | Combine a list of text with the word @or@.
disjunction :: [Doc] -> Doc
disjunction = hsep . intersperse "or"


-- | Compute the documentation fragment for an individual value specification.
valueDoc :: ValueSpec a -> DocBuilder Doc
valueDoc w =
  case w of
    TextSpec         -> pure "text"
    IntegerSpec      -> pure "integer"
    RationalSpec     -> pure "number"
    AtomSpec a       -> pure ("`" <> txt a <> "`")
    AnyAtomSpec      -> pure "atom"
    SectionSpecs l s -> sectionsDoc l s
    NamedSpec    l s -> emitDoc l =<< valuesDoc s
    CustomSpec l w'  -> (txt l                 <+>) <$> valuesDoc w'
    ListSpec ws      -> ("list of"             <+>) <$> valuesDoc ws
    AssocSpec ws     -> ("association list of" <+>) <$> valuesDoc ws


-- | A writer-like type. A mapping of section names and documentation
-- lines is accumulated.
newtype DocBuilder a = DocBuilder { runDocBuilder :: (Map Text Doc, a) }
  deriving (Functor, Applicative, Monad, Monoid, Show)


-- | Given a section name and section body, store the body
-- in the map of sections and return the section name.
emitDoc ::
  Text {- ^ section name -} ->
  Doc  {- ^ section body -} ->
  DocBuilder Doc
emitDoc l xs = DocBuilder (Map.singleton l xs, txt l)

------------------------------------------------------------------------

-- | Like text, but works on Text values.
txt :: Text -> Doc
txt = text . Text.unpack

-- | Like vcat but using ($+$) instead of ($$) to avoid overlap.
vcat' :: [Doc] -> Doc
vcat' = foldr ($+$) empty
