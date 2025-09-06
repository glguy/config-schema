{-# Language RecursiveDo, OverloadedStrings, GADTs, GeneralizedNewtypeDeriving, CPP #-}

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
configSpec :: ValueSpec (Text,Maybe Int)
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

import           Control.Applicative (liftA2)
import           Control.Monad (unless)
import           Control.Monad.Trans.State.Strict (runState, get, put, State)
import           Data.List (intersperse)
import           Data.List.NonEmpty (NonEmpty((:|)))
import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Semigroup as S
import           Data.Text (Text)
import qualified Data.Text as Text
import           Text.PrettyPrint
                    (Doc, fsep, text, (<>), ($+$), (<+>), nest, empty, hsep, parens)
import           Prelude hiding ((<>))

#if !MIN_VERSION_base(4,11,0)
import           Data.Monoid (Monoid(..))
#endif

import           Config
import           Config.Schema.Spec
import           Config.Schema.Types


data DocBodyKind = ShortBody | LongBody
type DocBody     = (DocBodyKind, Doc)


-- | Default documentation generator.
generateDocs :: ValueSpec a -> Doc
generateDocs spec = vcat' docLines
  where
    sectionLines :: (Text, DocBody) -> [Doc]
    sectionLines (name, (docKind, doc)) = [text "", txt name <> docHead] ++ docLines
        where
            (docHead, docLines) = case docKind of
                ShortBody -> (txt ": " <> doc, [])
                LongBody  -> (txt " fields: ", [nest 4 doc])

    (topDoc, topMap) = runDocBuilder (valuesDoc False spec)

    docLines =
      case runValueSpec_ (pure . SomeSpec) spec of
        -- single, top-level sections spec
        SomeSpec (SectionsSpec name _) :| []
          | Just top <- Map.lookup name topMap ->
              txt "Top-level configuration file fields:" :
              nest 4 (snd top) :
              concatMap sectionLines (Map.toList (Map.delete name topMap))

        -- otherwise
        _ -> txt "Top-level configuration file format:" :
             nest 4 topDoc :
             concatMap sectionLines (Map.toList topMap)


-- | Forget the type of the value spec
data SomeSpec where SomeSpec :: PrimValueSpec a -> SomeSpec


-- | Compute the documentation for a list of sections, store the
-- documentation in the sections map and return the name of the section.
sectionsDoc :: Text -> SectionsSpec a -> DocBuilder Doc
sectionsDoc l spec = emitDoc l LongBody (vcat' <$> runSections_ (fmap pure . sectionDoc) spec)


-- | Compute the documentation lines for a single key-value pair.
sectionDoc :: PrimSectionSpec a -> DocBuilder Doc
sectionDoc s =
  case s of
    ReqSection name desc w -> aux "REQUIRED" name desc <$> valuesDoc False w
    OptSection name desc w -> aux empty      name desc <$> valuesDoc False w
  where
    aux req name desc val =
      (txt name <> ":") <+> req <+> val $+$
      if Text.null desc
        then empty
        else nest 4 (fsep (txt <$> Text.splitOn " " desc)) -- line wrap logic


-- | Compute the documentation line for a particular value specification.
-- Any sections contained in the specification will be stored in the
-- sections map.
--
-- Set nested to 'True' when using valuesDoc in a nested context and
-- parentheses would be needed in the case of multiple alternatives.
valuesDoc :: Bool {- ^ nested -} -> ValueSpec a -> DocBuilder Doc
valuesDoc nested =
  fmap (disjunction nested) . sequenceA . runValueSpec_ (fmap pure valueDoc)


-- | Combine a list of text with the word @or@.
disjunction :: Bool {- ^ nested -} -> [Doc] -> Doc
disjunction _ [x]    = x
disjunction True  xs = parens (hsep (intersperse "OR" xs))
disjunction False xs =         hsep (intersperse "OR" xs)


-- | Compute the documentation fragment for an individual value specification.
valueDoc :: PrimValueSpec a -> DocBuilder Doc
valueDoc w =
  case w of
    TextSpec         -> pure "text"
    NumberSpec       -> pure "number"
    AtomSpec         -> pure "atom"
    SectionsSpec l s -> sectionsDoc l s
    NamedSpec    l s -> emitDoc l ShortBody (valuesDoc False s)
    CustomSpec l w'  -> (txt l                 <+>) <$> valuesDoc True w'
    ListSpec ws      -> ("list of"             <+>) <$> valuesDoc True ws
    AssocSpec ws     -> ("association list of" <+>) <$> valuesDoc True ws
    ExactSpec v      -> pure ("`" <> prettyInline v <> "`")

-- | A writer-like type. A mapping of section names and documentation
-- lines is accumulated.
newtype DocBuilder a = DocBuilder (State (Map Text DocBody) a)
  deriving (Functor, Applicative, Monad)

runDocBuilder :: DocBuilder a -> (a, Map Text DocBody)
runDocBuilder (DocBuilder b) = runState b mempty

-- | lifts underlying 'S.Semigroup' instance
instance S.Semigroup a => S.Semigroup (DocBuilder a) where
  (<>) = liftA2 (S.<>)

-- | lifts underlying 'Monoid' instance
instance (S.Semigroup a, Monoid a) => Monoid (DocBuilder a) where
  mempty  = pure mempty
  mappend = (S.<>)


-- | Given a section name and section body, store the body
-- in the map of sections and return the section name.
emitDoc ::
  Text           {- ^ section name      -} ->
  DocBodyKind    {- ^ section body kind -} ->
  DocBuilder Doc {- ^ section body      -} ->
  DocBuilder Doc {- ^ section name doc  -}
emitDoc l kind (DocBuilder body) = DocBuilder $
  do m <- get
     unless (Map.member l m) $
       do rec put $! Map.insert l (kind, body') m
              body' <- body
          return ()
     return (txt l)
  -- by using a recursively defined do block and
  -- inserting the element /before/ executing the @sub@
  -- action we ensure that @sub@ doesn't attempt to
  -- also explore elements named @l@

------------------------------------------------------------------------

-- | Like text, but works on Text values.
txt :: Text -> Doc
txt = text . Text.unpack

-- | Like vcat but using ($+$) instead of ($$) to avoid overlap.
vcat' :: [Doc] -> Doc
vcat' = foldr ($+$) empty
