{-# Language GADTs #-}

{-|
Module      : Config.Schema.Docs
Description : 
Copyright   : (c) Eric Mertens, 2017
License     : ISC
Maintainer  : emertens@gmail.com

-}
module Config.Schema.Docs where

import           Control.Applicative.Free         (runAp_)
import qualified Data.Text as Text

import           Config
import           Config.Schema.Spec

sectionsDoc :: SectionsSpec a -> [String]
sectionsDoc = runSections_ sectionDoc

sectionDoc :: SectionSpec a -> [String]
sectionDoc (ReqSection name desc w) =
  case valueDoc w of
    ShortDoc txt -> [Text.unpack name ++ " :: " ++ txt ++ "; " ++ Text.unpack desc]
    LongDoc txts ->
      (Text.unpack name ++ " :: section; " ++ Text.unpack desc)
      : map ("    "++)txts
sectionDoc (OptSection name desc w) =
  case valueDoc w of
    ShortDoc txt -> [Text.unpack name ++ " :: optional " ++ txt ++ "; " ++ Text.unpack desc]
    LongDoc txts ->
      (Text.unpack name ++ " :: optional section; " ++ Text.unpack desc)
      : map ("    "++)txts

data DocLines = ShortDoc String | LongDoc [String]

valueDoc :: ValueSpec a -> DocLines
valueDoc w =
  case w of
    ListSpec ws ->
      case valueDoc ws of
        ShortDoc x -> ShortDoc ("list of " ++ x)
        LongDoc xs -> LongDoc ("list of" : map ("    "++) xs)
    TextSpec   -> ShortDoc "text"
    NumberSpec -> ShortDoc "number"
    AtomSpec a -> ShortDoc ("`" ++ Text.unpack (atomName a) ++ "`")
    ChoiceSpec wa wb ->
      case (valueDoc wa, valueDoc wb) of
        (ShortDoc x, ShortDoc y) -> ShortDoc (x ++ " or " ++ y)
    SectionsSpec s -> LongDoc (sectionsDoc s)
    MapSpec _ w'  -> valueDoc w'
