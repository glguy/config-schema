{-# Language GADTs #-}

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
valuesDoc = flattenOptions . getChoices . runValuesSpec (Choices . pure . pure . valueDoc)

flattenOptions :: [[DocLines]] -> DocLines
flattenOptions [[LongDoc x]] = LongDoc x
flattenOptions xs
  = ShortDoc
  . intercalate " or "
  . map (intercalate " and " . map simpleDocString)
  $ xs

simpleDocString :: DocLines -> String
simpleDocString (ShortDoc x) = x
simpleDocString (LongDoc _) = "complex"

valueDoc :: ValueSpec a -> DocLines
valueDoc w =
  case w of
    ListSpec ws ->
      case valuesDoc ws of
        ShortDoc x -> ShortDoc ("list of " ++ x)
        LongDoc xs -> LongDoc ("list of" : map ("    "++) xs)
    TextSpec   -> ShortDoc "text"
    NumberSpec -> ShortDoc "number"
    AtomSpec a -> ShortDoc ("`" ++ Text.unpack (atomName a) ++ "`")
    SectionsSpec s -> LongDoc (sectionsDoc s)


------------------------------------------------------------------------
-- Gathered choices
------------------------------------------------------------------------

newtype Choices f a x = Choices { getChoices :: f a }

instance Functor (Choices f a) where
  fmap _ (Choices x) = Choices x

instance (Applicative f, Monoid m) => Applicative (Choices f m) where
  pure _                  = Choices (pure mempty)
  Choices x <*> Choices y = Choices (liftA2 mappend x y)

instance (Alternative f, Monoid m) => Alternative (Choices f m) where
  empty                   = Choices empty
  Choices x <|> Choices y = Choices (x <|> y)
