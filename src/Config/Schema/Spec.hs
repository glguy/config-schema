{-# LANGUAGE RankNTypes, GADTs, KindSignatures, GeneralizedNewtypeDeriving #-}

{-|
Module      : Config.Schema.Spec
Description : 
Copyright   : (c) Eric Mertens, 2017
License     : ISC
Maintainer  : emertens@gmail.com

-}
module Config.Schema.Spec where

import Config
import Control.Applicative              (Alternative(..))
import Control.Applicative.Free         (Ap, runAp, runAp_, liftAp)
import Control.Alternative.Free         (Alt, liftAlt, runAlt)
import Data.Monoid                      ((<>))
import Data.Text                        (Text)
import Data.Functor.Coyoneda
import qualified Data.Text as Text

------------------------------------------------------------------------
-- Specifications for sections
------------------------------------------------------------------------

data SectionSpec :: * -> * where
  ReqSection :: Text -> Text -> ValuesSpec a -> SectionSpec a
  OptSection :: Text -> Text -> ValuesSpec a -> SectionSpec (Maybe a)


newtype SectionsSpec a = MkSectionsSpec (Ap SectionSpec a)
  deriving (Functor, Applicative)


sectionSpec :: SectionSpec a -> SectionsSpec a
sectionSpec = MkSectionsSpec . liftAp


runSections :: Applicative f => (forall x. SectionSpec x -> f x) -> SectionsSpec a -> f a
runSections f (MkSectionsSpec s) = runAp f s


runSections_ :: Monoid m => (forall x. SectionSpec x -> m) -> SectionsSpec a -> m
runSections_ f (MkSectionsSpec s) = runAp_ f s


------------------------------------------------------------------------
-- 'SectionsSpec' builders
------------------------------------------------------------------------


reqSection ::
  Spec a =>
  Text {- ^ section name -} ->
  Text {- ^ description  -} ->
  SectionsSpec a
reqSection n i = sectionSpec (ReqSection n i valuesSpec)


reqSection' ::
  Text         {- ^ section name  -} ->
  Text         {- ^ description   -} ->
  ValuesSpec a {- ^ value matcher -} ->
  SectionsSpec a
reqSection' n i w = sectionSpec (ReqSection n i w)


optSection ::
  Spec a =>
  Text {- ^ section name -} ->
  Text {- ^ description  -} ->
  SectionsSpec (Maybe a)
optSection n i = sectionSpec (OptSection n i valuesSpec)


optSection' ::
  Text         {- ^ section name  -} ->
  Text         {- ^ description   -} ->
  ValuesSpec a {- ^ value matcher -} ->
  SectionsSpec (Maybe a)
optSection' n i w = sectionSpec (OptSection n i w)


------------------------------------------------------------------------
-- Specifications for values
------------------------------------------------------------------------

data ValueSpec :: * -> * where
  ListSpec     :: ValuesSpec a -> ValueSpec [a]
  TextSpec     :: ValueSpec Text
  NumberSpec   :: ValueSpec Integer
  AtomSpec     :: Atom -> ValueSpec ()
  SectionsSpec :: SectionsSpec a -> ValueSpec a


newtype ValuesSpec a = MkValuesSpec (Alt (Coyoneda ValueSpec) a)
  deriving (Functor, Applicative, Alternative)


runValuesSpec :: Alternative f => (forall x. ValueSpec x -> f x) -> ValuesSpec x -> f x
runValuesSpec f (MkValuesSpec s) = runAlt (\(Coyoneda g x) -> fmap g (f x))  s


valueSpec :: ValueSpec a -> ValuesSpec a
valueSpec = MkValuesSpec . liftAlt . liftCoyoneda


------------------------------------------------------------------------
-- 'ValuesSpec' builders
------------------------------------------------------------------------


-- | Class for things that are easy to want
class    Spec a       where valuesSpec :: ValuesSpec a
instance Spec Text    where valuesSpec = valueSpec TextSpec
instance Spec Integer where valuesSpec = valueSpec NumberSpec
instance Spec a => Spec [a] where valuesSpec = valueSpec (ListSpec valuesSpec)
instance (Spec a, Spec b) => Spec (Either a b) where valuesSpec = valuesSpec <|> valuesSpec


atomSpec :: Text -> ValuesSpec ()
atomSpec = valueSpec . AtomSpec . MkAtom


listSpec :: ValuesSpec a -> ValuesSpec [a]
listSpec = valueSpec . ListSpec


sectionsSpec :: SectionsSpec a -> ValuesSpec a
sectionsSpec = valueSpec . SectionsSpec


oneOrList :: ValuesSpec a -> ValuesSpec [a]
oneOrList s = (:[]) <$> s <|> listSpec s
