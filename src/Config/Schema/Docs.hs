{-# Language GADTs, GeneralizedNewtypeDeriving #-}

{-|
Module      : Config.Schema.Docs
Description : 
Copyright   : (c) Eric Mertens, 2017
License     : ISC
Maintainer  : emertens@gmail.com

-}
module Config.Schema.Docs where

import           Control.Applicative
import           Control.Applicative.Free         (runAp_)
import qualified Data.Text as Text
import           Data.Functor.Const
import           Data.Functor.Compose
import           Data.List

import           Config
import           Config.Schema.Spec

sectionsDoc :: SectionsSpec a -> [String]
sectionsDoc = runSections_ sectionDoc

sectionDoc :: SectionSpec a -> [String]
sectionDoc (ReqSection name desc w) =
  case valuesDoc w of
    ShortDoc txt -> [Text.unpack name ++ " :: " ++ txt ++ "; " ++ Text.unpack desc]
    LongDoc txts ->
      (Text.unpack name ++ " :: section; " ++ Text.unpack desc)
      : map ("    "++)txts
sectionDoc (OptSection name desc w) =
  case valuesDoc w of
    ShortDoc txt -> [Text.unpack name ++ " :: optional " ++ txt ++ "; " ++ Text.unpack desc]
    LongDoc txts ->
      (Text.unpack name ++ " :: optional section; " ++ Text.unpack desc)
      : map ("    "++)txts

data DocLines = ShortDoc String | LongDoc [String]

valuesDoc :: ValuesSpec a -> DocLines
valuesDoc = flattenOptions . lowerChoices . runValuesSpec (liftChoices . inflateOptions . valueDoc)

inflateOptions :: DocLines -> [[DocLines]]
inflateOptions x = [[x]]

flattenOptions :: [[DocLines]] -> DocLines
flattenOptions [[LongDoc x]] = LongDoc x
flattenOptions xs
  = ShortDoc
  . intercalate " or "
  . map (conjunction . map simpleDocString)
  $ xs


-- | Explain a conjunction of docstrings
conjunction :: [String] -> String
conjunction [] = "ignored"
conjunction xs = intercalate " and " xs


simpleDocString :: DocLines -> String
simpleDocString (ShortDoc x) = x
simpleDocString (LongDoc _) = "complex"

valueDoc :: ValueSpec a -> DocLines
valueDoc w =
  case w of
    TextSpec       -> ShortDoc "text"
    NumberSpec     -> ShortDoc "number"
    AtomSpec a     -> ShortDoc ("`" ++ Text.unpack (atomName a) ++ "`")
    SectionsSpec s -> LongDoc (sectionsDoc s)
    ListSpec ws    ->
      case valuesDoc ws of
        ShortDoc x -> ShortDoc ("list of " ++ x)
        LongDoc xs -> LongDoc ("list of" : map ("    "++) xs)


------------------------------------------------------------------------
-- Gathered choices
------------------------------------------------------------------------

newtype Choices f m a = Choices { unChoices :: Compose f (Const m) a }
  deriving (Functor, Applicative, Alternative)

liftChoices :: Functor f => f m -> Choices f m a
liftChoices = Choices . Compose . fmap Const

lowerChoices :: Functor f => Choices f m a -> f m
lowerChoices = fmap getConst . getCompose . unChoices
