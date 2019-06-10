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

The structure is you get a single value and a list of one-or-more
primitive specifications that it failed to match along with
an enumeration of why that specification failed to match. Some
failures are due to failures in nested specifications, so the
whole error structure can form a tree.

-}
module Config.Schema.Load.Error
  (
  -- * Error types
    ValueSpecMismatch(..)
  , PrimMismatch(..)
  , Problem(..)
  , ErrorAnnotation(..)

  -- * Detailed rendering
  , prettyValueSpecMismatch
  , prettyPrimMismatch
  , prettyProblem

  -- * Summaries
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
                    (Doc, fsep, ($+$), nest, text, vcat, (<+>), empty,
                     punctuate, comma, int, colon, hcat)
import           Data.Monoid ((<>))

import           Config
import           Config.Schema.Types

-- | Newtype wrapper for schema load errors.
data ValueSpecMismatch p =
  -- | Problem value and list of specification failures
  ValueSpecMismatch (Value p) (NonEmpty (PrimMismatch p))
  deriving Show

-- | Type for errors that can be encountered while decoding a value according
-- to a specification. The error includes a key path indicating where in
-- the configuration file the error occurred.
data PrimMismatch p =
  -- | spec description and problem
  PrimMismatch Text (Problem p)
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

-- | Describe outermost shape of a 'PrimValueSpec'
describeSpec :: PrimValueSpec a -> Text
describeSpec TextSpec                   = "text"
describeSpec IntegerSpec                = "integer"
describeSpec RationalSpec               = "number"
describeSpec AnyAtomSpec                = "atom"
describeSpec (AtomSpec a)               = "atom `" <> a <> "`"
describeSpec (ListSpec _)               = "list"
describeSpec (SectionsSpec name _)      = name
describeSpec (AssocSpec _)              = "sections"
describeSpec (CustomSpec name _)        = name
describeSpec (NamedSpec name _)         = name

-- | Describe outermost shape of a 'Value'
describeValue :: Value p -> String
describeValue Text{}     = "text"
describeValue Number{}   = "integer"
describeValue Floating{} = "number"
describeValue (Atom _ a) = "atom `" <> Text.unpack (atomName a) <> "`"
describeValue Sections{} = "sections"
describeValue List{}     = "list"

-- | Pretty-printer for 'ValueSpecMismatch' showing the position
-- and type of value that failed to match along with details about
-- each specification that it didn't match.
prettyValueSpecMismatch :: ErrorAnnotation p => ValueSpecMismatch p -> Doc
prettyValueSpecMismatch (ValueSpecMismatch v es) =
  heading $+$ nest 4 errors
  where
    heading = displayAnnotation (valueAnn v)
          <+> text (describeValue v)
          <+> text reason
    errors = vcat (map prettyPrimMismatch (toList es))

    reason
      | _ :| [] <- es = "failed to match spec:"
      | otherwise     = "failed to match specs:"


-- | Pretty-printer for 'PrimMismatch' showing a summary of the primitive
-- specification that didn't match followed by a more detailed error when
-- appropriate.
prettyPrimMismatch :: ErrorAnnotation p => PrimMismatch p -> Doc
prettyPrimMismatch (PrimMismatch spec problem) =
  case prettyProblem problem of
    (summary, detail) ->
      (text "*" <+> text (Text.unpack spec) <+> summary) $+$ nest 4 detail

-- | Pretty-printer for 'Problem' that generates a summary line
-- as well as a detailed description (depending on the error)
prettyProblem ::
  ErrorAnnotation p =>
  Problem p ->
  (Doc, Doc) {- ^ summary, detailed -}
prettyProblem p =
  case p of
    TypeMismatch -> (empty, empty)
    MissingSection name ->
      ( text "- missing section:" <+> text (Text.unpack name)
      , empty)
    UnusedSections names ->
      ( text "- unexpected sections:" <+>
        fsep (punctuate comma (map (text . Text.unpack) (toList names)))
      , empty)
    CustomProblem e ->
      ( text "-" <+> text (Text.unpack e)
      , empty)
    SubkeyProblem name e ->
      ( text "- problem in section:" <+> text (Text.unpack name)
      , prettyValueSpecMismatch e)
    NestedProblem e ->
      ( empty
      , prettyValueSpecMismatch e)
    ListElementProblem i e ->
      ( text "- problem in element:" <+> int i
      , prettyValueSpecMismatch e)

-- | Class for rendering position annotations within the 'prettyValueSpecMismatch'
class (Typeable a, Show a) => ErrorAnnotation a where
  displayAnnotation :: a -> Doc

-- | Renders a 'Position' as @line:column:@
instance ErrorAnnotation Position where
  displayAnnotation pos = hcat [int (posLine pos), colon, int (posColumn pos), colon]

-- | Renders as an empty document
instance ErrorAnnotation () where
  displayAnnotation _ = empty

-- | 'displayException' implemented with 'prettyValueSpecMismatch'
instance ErrorAnnotation p => Exception (ValueSpecMismatch p) where
  displayException = show . prettyValueSpecMismatch
