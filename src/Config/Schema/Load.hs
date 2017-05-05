{-# Language GADTs #-}
{-|
Module      : Config.Schema.Load
Description : 
Copyright   : (c) Eric Mertens, 2017
License     : ISC
Maintainer  : emertens@gmail.com

-}
module Config.Schema.Load where

import Control.Applicative              ((<|>), optional)
import Control.Monad.Trans.State        (StateT(..), runStateT)
import Control.Monad                    (guard)
import Control.Monad.Trans.Class        (lift)
import Control.Applicative.Free         (runAp)
import Data.Text                        (Text)

import Config.Schema.Spec
import Config

getSection :: SectionSpec a -> StateT [Section] Maybe a
getSection (ReqSection k _ w) =
  do v <- StateT (lookupSection k)
     lift (getValue w v)
getSection (OptSection k _ w) =
  do mb <- optional (StateT (lookupSection k))
     lift (traverse (getValue w) mb)


getSections :: SectionsSpec a -> [Section] -> Maybe a
getSections p xs =
  do (a,[]) <- runStateT (runSections getSection p) xs
     return a


-- | Extract a section from a list of sections by name.
lookupSection ::
  Text                     {- ^ section name                       -} ->
  [Section]                {- ^ available sections                 -} ->
  Maybe (Value, [Section]) {- ^ found value and remaining sections -}
lookupSection _ [] = Nothing
lookupSection key (s@(Section k v):xs)
  | key == k  = Just (v, xs)
  | otherwise = do (v',xs') <- lookupSection key xs
                   return (v',s:xs')


getValue :: ValueSpec a -> Value -> Maybe a
getValue TextSpec         (Text t)     = Just t
getValue NumberSpec       (Number _ n) = Just n
getValue (ListSpec w)     (List xs)    = traverse (getValue w) xs
getValue (AtomSpec a)     (Atom b)     = guard (a == b)
getValue (ChoiceSpec a b) v            = Left <$> getValue a v <|> Right <$> getValue b v
getValue (SectionsSpec w) (Sections s) = getSections w s
getValue (MapSpec f w)    v            = fmap f (getValue w v)
getValue _                _            = Nothing

