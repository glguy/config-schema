{-# Language OverloadedStrings, ApplicativeDo #-}
module Example where

import           Control.Applicative
import qualified Data.Text as Text
import           Data.Text (Text)
import           Data.Monoid ((<>))

import           Config
import           Config.Schema

exampleFile :: Text
exampleFile =
  " name: \"Johny Appleseed\" \n\
  \ age : 99                  \n\
  \ happy: yes                \n\
  \ kids:                     \n\
  \   * \"Bob\"               \n\
  \   * \"Tom\"               \n"

exampleValue :: Value
Right exampleValue = parse exampleFile

exampleSpec :: SectionsSpec Text
exampleSpec =
  do name  <- reqSection  "name" "Full name"
     age   <- reqSection  "age"  "Age of user"
     happy <- optSection' "happy" "Current happiness status" yesOrNo
     kids  <- reqSection' "kids"  "All children" (oneOrList valuesSpec)

     return $
       let happyText = case happy of Just True  -> " and is happy"
                                     Just False -> " and is not happy"
                                     Nothing    -> " and is private"

       in name <> " is " <> Text.pack (show (age::Integer)) <>
             " years old and has kids " <>
             Text.intercalate ", " kids <>
             happyText

-- | Matches the 'yes' and 'no' atoms
yesOrNo :: ValuesSpec Bool
yesOrNo = True  <$ atomSpec "yes" <|>
          False <$ atomSpec "no"


printDoc :: IO ()
printDoc = putStr $ unlines $ sectionsDoc exampleSpec
-- *Example> printDoc
-- name :: text; Full name
-- age :: number; Age of user
-- happy :: optional `yes` or `no`; Current happiness status
-- kids :: text or list of text; All children

example :: Either [LoadError] Text
example = loadSections exampleSpec exampleValue
-- *Example> exampleVal
-- Right "Johny Appleseed is 99 years old and has kids Bob, Tom and is happy"
