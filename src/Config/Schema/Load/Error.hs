{-# Language GADTs, OverloadedStrings #-}
{-|
Module      : Config.Schema.Load.Error
Description : Error types and rendering for Load module
Copyright   : (c) Eric Mertens, 2019
License     : ISC
Maintainer  : emertens@gmail.com

This module provides a complete skeleton of the failures that
occurred when trying to match a 'Value' against a 'ValueSpec'
allowing custom error rendering to be implemented.

-}
module Config.Schema.Load.Error
  (
  -- * Error types
    ValueSpecMismatch(..)
  , PrimMismatch(..)
  , Problem(..)

  -- * Rendering
  , prettyValueSpecMismatch
  , prettyPrimMismatch
  , prettyProblem

  , describeSpec
  , describeValue
  ) where

import           Control.Exception
import           Data.Text (Text)
import           Data.Foldable (toList)
import qualified Data.Text as Text
import           Data.List.NonEmpty (NonEmpty((:|)))
import           Data.Typeable (Typeable)
import           Text.PrettyPrint
                    (Doc, fsep, ($+$), nest, text, vcat, (<+>), empty, punctuate, comma)

import           Config
import           Config.Schema.Types

-- | Newtype wrapper for schema load errors.
data ValueSpecMismatch p =
  ValueSpecMismatch (Value p) (NonEmpty (PrimMismatch p))
  deriving Show

-- | Type for errors that can be encountered while decoding a value according
-- to a specification. The error includes a key path indicating where in
-- the configuration file the error occurred.
data PrimMismatch p =
  PrimMismatch Text (Problem p) -- ^ spec description and problem
  deriving Show


-- | Problems that can be encountered when matching a 'Value' against a 'ValueSpec'.
data Problem p
  = MissingSection Text                          -- ^ missing section name
  | UnusedSections (NonEmpty Text)               -- ^ unused section names
  | SubkeyProblem Text     (ValueSpecMismatch p) -- ^ nested error in given section
  | ListElementProblem Int (ValueSpecMismatch p) -- ^ nested error in given list element
  | NestedProblem          (ValueSpecMismatch p) -- ^ generic nested error
  | TypeMismatch                                 -- ^ value and spec type mismatch
  | CustomProblem Text                           -- ^ custom spec error message
  deriving Show

describeSpec :: PrimValueSpec a -> Text
describeSpec TextSpec              = "text"
describeSpec IntegerSpec           = "integer"
describeSpec RationalSpec          = "number"
describeSpec AnyAtomSpec           = "atom"
describeSpec (AtomSpec a)          = "`" <> a <> "`"
describeSpec (ListSpec _)          = "list"
describeSpec (SectionsSpec name _) = name
describeSpec (AssocSpec _)         = "association list"
describeSpec (CustomSpec name _)   = name
describeSpec (NamedSpec name _)    = name

describeValue :: Value p -> String
describeValue Text{}     = "text"
describeValue Number{}   = "integer"
describeValue Floating{} = "number"
describeValue (Atom _ a) = "`" <> Text.unpack (atomName a) <> "`"
describeValue Sections{} = "sections"
describeValue List{}     = "list"


prettyValueSpecMismatch :: ErrorAnnotation p => ValueSpecMismatch p -> Doc
prettyValueSpecMismatch (ValueSpecMismatch v es) =
  heading $+$ nest 4 errors
  where
    heading = text (displayAnnotation (valueAnn v))
          <+> text (describeValue v)
          <+> text reason
    errors = vcat (map prettyPrimMismatch (toList es))

    reason
      | _ :| [] <- es = "failed to match spec:"
      | otherwise     = "failed to match specs:"


prettyPrimMismatch :: ErrorAnnotation p => PrimMismatch p -> Doc
prettyPrimMismatch (PrimMismatch spec problem) =
  case prettyProblem problem of
    (summary, detail) ->
      (text "*" <+> text (Text.unpack spec) <+> summary) $+$ nest 4 detail

prettyProblem :: ErrorAnnotation p => Problem p -> (Doc, Doc)
prettyProblem p =
  case p of
    MissingSection name -> (text "- missing section:" <+> text (Text.unpack name), empty)
    UnusedSections names -> (text "- unexpected sections:"
                        <+> fsep (punctuate comma (map (text . Text.unpack) (toList names))), empty)
    CustomProblem e -> (text (Text.unpack e), empty)
    TypeMismatch  -> (empty, empty)
    SubkeyProblem name e ->
      (text "- problem in section:" <+> text (Text.unpack name), prettyValueSpecMismatch e)
    NestedProblem e -> (empty, prettyValueSpecMismatch e)
    ListElementProblem i e ->
      (text "- problem in element:" <+> text (show i), prettyValueSpecMismatch e)

class (Typeable a, Show a) => ErrorAnnotation a where
  displayAnnotation :: a -> String

instance ErrorAnnotation Position where
  displayAnnotation pos = show (posLine pos) ++ ":" ++ show (posColumn pos) ++ ":"

-- | 'displayException' implemented with 'prettyValueSpecMismatch'
instance ErrorAnnotation p => Exception (ValueSpecMismatch p) where
  displayException = show . prettyValueSpecMismatch
