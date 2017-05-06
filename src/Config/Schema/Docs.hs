{-# Language OverloadedStrings, GADTs, GeneralizedNewtypeDeriving #-}

{-|
Module      : Config.Schema.Docs
Description : Documentation generation for config schemas
Copyright   : (c) Eric Mertens, 2017
License     : ISC
Maintainer  : emertens@gmail.com

-}
module Config.Schema.Docs
  ( generateDocs
  , generateDocs'
  , KeyValueDoc(..)
  ) where

import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as Text

import           Config.Schema.Spec

-- | Default documentation generator. This generator is specifically
-- for configuration specifications where the top-level specification
-- is named with the empty string.
--
-- For more control, use 'generateDocs''
generateDocs :: ValueSpecs a -> Text
generateDocs spec = Text.unlines
  ("Configuration file format:"
   : concatMap fieldLines top
  ++ concatMap sectionLines (Map.toList (Map.delete topname m)))

  where
    topname = ""
    Just top = Map.lookup topname m
    DocBuilder (m,"") = valuesDoc spec

    sectionLines (name, fields)
      = ""
      : name
      : concatMap fieldLines fields

    fieldLines (NamedDoc ty) = ["    " <> ty]
    fieldLines (KeyValueDoc isReq field desc ty) =
      ("    " <> (if isReq then "req " else "") <> field <> ": " <> ty)
      : if Text.null desc then [] else ["       " <> desc]


generateDocs' :: ValueSpecs a -> (Map Text [KeyValueDoc], Text)
generateDocs' spec = x
  where DocBuilder x = valuesDoc spec

sectionsDoc :: SectionSpecs a -> DocBuilder [KeyValueDoc]
sectionsDoc spec = runSections_ (fmap pure . sectionDoc) spec

sectionDoc :: SectionSpec a -> DocBuilder KeyValueDoc
sectionDoc (ReqSection name desc w) = KeyValueDoc True name desc <$> valuesDoc w
sectionDoc (OptSection name desc w) = KeyValueDoc False name desc <$> valuesDoc w -- XXX mark optional

valuesDoc :: ValueSpecs a -> DocBuilder Text
valuesDoc =
  fmap flattenOptions . foldMap (fmap (:[])) . runValueSpecs_ (fmap (:[]) . valueDoc)


flattenOptions :: [[Text]] -> Text
flattenOptions [] = "impossible"
flattenOptions xs = Text.intercalate " or " (map conjunction xs)


-- | Explain a conjunction of docstrings
conjunction :: [Text] -> Text
conjunction [] = "ignored"
conjunction xs = Text.intercalate " and " xs


valueDoc :: ValueSpec a -> DocBuilder Text
valueDoc w =
  case w of
    TextSpec         -> docBuilder "text"
    IntegerSpec      -> docBuilder "integer"
    RationalSpec     -> docBuilder "number"
    AtomSpec a       -> docBuilder ("`" <> a <> "`")
    AnyAtomSpec      -> docBuilder "atom"

    SectionSpecs l s -> DocBuilder (Map.insert l xs m, l)
        where DocBuilder (m,xs) = sectionsDoc s

    NamedSpec l s -> DocBuilder (Map.insert l [NamedDoc xs] m, l)
        where DocBuilder (m,xs) = valuesDoc s

    CustomSpec l w' _ -> ((l <> " ") <>) <$> valuesDoc w'
    ListSpec ws -> ("list of " <>) <$> valuesDoc ws

data KeyValueDoc
  = KeyValueDoc Bool Text Text Text
  | NamedDoc Text
  deriving (Read,Show)

newtype DocBuilder a = DocBuilder (Map Text [KeyValueDoc], a)
  deriving (Functor, Monoid, Read, Show)

docBuilder :: a -> DocBuilder a
docBuilder x = DocBuilder (mempty, x)
