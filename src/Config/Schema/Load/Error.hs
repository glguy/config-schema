{-# Language GADTs, OverloadedStrings, CPP #-}
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
  , simplifyValueSpecMismatch
  ) where

import           Control.Exception
import           Data.Text (Text)
import           Data.Foldable (toList)
import qualified Data.Text as Text
import           Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Typeable (Typeable)
import           Text.PrettyPrint
                    (Doc, fsep, ($+$), nest, text, vcat, (<+>), empty,
                     punctuate, comma, int, colon, hcat)

import           Config
import           Config.Macro (FilePosition(..))
import           Config.Schema.Types

#if !MIN_VERSION_base(4,11,0)
import           Data.Monoid ((<>))
#endif

-- | Newtype wrapper for schema load errors.
--
-- @since 1.2.0.0
data ValueSpecMismatch p =
  -- | Problem value and list of specification failures
  ValueSpecMismatch p Text (NonEmpty (PrimMismatch p))
  deriving Show

-- | Type for errors that can be encountered while decoding a value according
-- to a specification. The error includes a key path indicating where in
-- the configuration file the error occurred.
--
-- @since 1.2.0.0
data PrimMismatch p =
  -- | spec description and problem
  PrimMismatch Text (Problem p)
  deriving Show


-- | Problems that can be encountered when matching a 'Value' against a 'ValueSpec'.
--
-- @since 1.2.0.0
data Problem p
  = MissingSection Text                          -- ^ missing section name
  | UnusedSections (NonEmpty Text)               -- ^ unused section names
  | SubkeyProblem Text     (ValueSpecMismatch p) -- ^ nested error in given section
  | ListElementProblem Int (ValueSpecMismatch p) -- ^ nested error in given list element
  | NestedProblem          (ValueSpecMismatch p) -- ^ generic nested error
  | TypeMismatch                                 -- ^ value and spec type mismatch
  | CustomProblem Text                           -- ^ custom spec error message
  | WrongExact                                   -- ^ values didn't match
  deriving Show

-- | Describe outermost shape of a 'PrimValueSpec'
--
-- @since 1.2.0.0
describeSpec :: PrimValueSpec a -> Text
describeSpec TextSpec                   = "text"
describeSpec NumberSpec                 = "number"
describeSpec AtomSpec                   = "atom"
describeSpec (ListSpec _)               = "list"
describeSpec (SectionsSpec name _)      = name
describeSpec (AssocSpec _)              = "sections"
describeSpec (CustomSpec name _)        = name
describeSpec (NamedSpec name _)         = name
describeSpec (ExactSpec (Atom _ a))     = "atom `" <> atomName a <> "`"
describeSpec (ExactSpec v)              = Text.pack (show (pretty v))

-- | Describe outermost shape of a 'Value'
describeValue :: Value p -> Text
describeValue Text{}     = "text"
describeValue Number{}   = "number"
describeValue (Atom _ a) = "atom `" <> atomName a <> "`"
describeValue Sections{} = "sections"
describeValue List{}     = "list"

-- | Bottom-up transformation of a 'ValueSpecMismatch'
rewriteMismatch ::
  (ValueSpecMismatch p -> ValueSpecMismatch p) ->
  ValueSpecMismatch p -> ValueSpecMismatch p
rewriteMismatch f (ValueSpecMismatch p v prims) = f (ValueSpecMismatch p v (fmap aux1 prims))
  where
    aux1 (PrimMismatch spec prob) = PrimMismatch spec (aux2 prob)

    aux2 (SubkeyProblem      x y) = SubkeyProblem      x (rewriteMismatch f y)
    aux2 (ListElementProblem x y) = ListElementProblem x (rewriteMismatch f y)
    aux2 (NestedProblem        y) = NestedProblem        (rewriteMismatch f y)
    aux2 prob                     = prob


-- | Single-step rewrite that removes type-mismatch problems if there
-- are non-mismatches available to focus on.
removeTypeMismatch1 :: ValueSpecMismatch p -> ValueSpecMismatch p
removeTypeMismatch1 (ValueSpecMismatch p v xs)
  | Just xs' <- NonEmpty.nonEmpty (NonEmpty.filter (not . isTypeMismatch) xs)
  = ValueSpecMismatch p v xs'
removeTypeMismatch1 v = v

-- | Returns 'True' for schema mismatches where the value type doesn't
-- match.
isTypeMismatch :: PrimMismatch p -> Bool
isTypeMismatch (PrimMismatch _ prob) =
  case prob of
    WrongExact                               -> True
    TypeMismatch                             -> True
    NestedProblem (ValueSpecMismatch _ _ xs) -> all isTypeMismatch xs
    _                                        -> False

-- | Single-step rewrite that removes mismatches with only a single,
-- nested mismatch below them.
focusMismatch1 :: ValueSpecMismatch p -> ValueSpecMismatch p
focusMismatch1 x@(ValueSpecMismatch _ _ prims)
  | PrimMismatch _ problem :| [] <- prims
  , Just sub <- simplify1 problem = sub
  | otherwise = x
  where
    simplify1 (SubkeyProblem      _ p) = Just p
    simplify1 (ListElementProblem _ p) = Just p
    simplify1 (NestedProblem        p) = Just p
    simplify1 _                        = Nothing


-- | Pretty-printer for 'ValueSpecMismatch' showing the position
-- and type of value that failed to match along with details about
-- each specification that it didn't match.
--
-- @since 1.2.0.0
prettyValueSpecMismatch :: ErrorAnnotation p => ValueSpecMismatch p -> Doc
prettyValueSpecMismatch (ValueSpecMismatch p v es) =
  heading $+$ errors
  where
    heading = displayAnnotation p <> text (Text.unpack v)
    errors = vcat (map prettyPrimMismatch (toList es))


-- | Pretty-printer for 'PrimMismatch' showing a summary of the primitive
-- specification that didn't match followed by a more detailed error when
-- appropriate.
--
-- @since 1.2.0.0
prettyPrimMismatch :: ErrorAnnotation p => PrimMismatch p -> Doc
prettyPrimMismatch (PrimMismatch spec problem) =
  case prettyProblem problem of
    (summary, detail) ->
      (text "* expected" <+> text (Text.unpack spec) <+> summary) $+$ nest 4 detail

-- | Simplify a 'ValueSpecMismatch' by collapsing long nested error
-- cases and by assuming that if a type matched that the other mismatched
-- type alternatives are uninteresting. This is used in the implementation
-- of 'displayException'.
--
-- @since 1.2.1.0
simplifyValueSpecMismatch :: ValueSpecMismatch p -> ValueSpecMismatch p
simplifyValueSpecMismatch = rewriteMismatch (focusMismatch1 . removeTypeMismatch1)

-- | Pretty-printer for 'Problem' that generates a summary line
-- as well as a detailed description (depending on the error)
--
-- @since 1.2.0.0
prettyProblem ::
  ErrorAnnotation p =>
  Problem p ->
  (Doc, Doc) {- ^ summary, detailed -}
prettyProblem p =
  case p of
    TypeMismatch ->
      ( text "- type mismatch"
      , empty)
    WrongExact ->
      ( text "- wrong value"
      , empty)
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
--
-- @since 1.2.0.0
class (Typeable a, Show a) => ErrorAnnotation a where
  displayAnnotation :: a -> Doc

-- | Renders a 'Position' as @line:column:@
--
-- @since 1.2.0.0
instance ErrorAnnotation Position where
  displayAnnotation pos = hcat [int (posLine pos), colon, int (posColumn pos), colon]

instance ErrorAnnotation FilePosition where
  displayAnnotation (FilePosition path pos) = hcat [text path, colon, int (posLine pos), colon, int (posColumn pos), colon]

-- | Renders as an empty document
--
-- @since 1.2.0.0
instance ErrorAnnotation () where
  displayAnnotation _ = empty

-- | 'displayException' implemented with 'prettyValueSpecMismatch'
-- and 'simplifyValueSpecMismatch'.
--
-- @since 1.2.0.0
instance ErrorAnnotation p => Exception (ValueSpecMismatch p) where
  displayException = show . prettyValueSpecMismatch . simplifyValueSpecMismatch
