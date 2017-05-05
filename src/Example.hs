{-# Language OverloadedStrings, ApplicativeDo #-}
module Example where

import qualified Data.Text as Text
import           Data.Text (Text)
import           Data.Monoid ((<>))

import           Config
import           Config.Schema

exampleFile :: Text
exampleFile =
  " name: \"Johny Appleseed\"\n\
  \ age : 99\n\
  \ happy: yes\n\
  \ kids:\n\
  \   * name: \"Bob\"\n\
  \   * name: \"Tom\"\n"

example :: [Section]
Right (Sections example) = parse exampleFile

-- *Demo Config> fromConfig example exampleSpec
-- Just "Johny Appleseed is 99 years old and has kids: Bob, Tom and is happy"
--
-- *Demo Config> putStr $ unlines $ configDocLines exampleSpec
-- name :: text; Full name
-- age :: number; Age of user
-- happy :: `yes` or `no`; Current happiness status
-- kids :: section; All children
--     list of
--         name :: text; Name of the kid

exampleSpec :: SectionsSpec Text
exampleSpec =
  do name  <- section "name" "Full name"
     age   <- section "age"  "Age of user"
     happy <- section' "happy" "Current happiness status" yesOrNo
     kids  <- section' "kids" "All children" (ListSpec (SectionsSpec kidSpec))

     return (name <> " is " <> Text.pack (show (age::Integer)) <>
             " years old and has kids: " <>
             Text.intercalate ", " kids <>
             if happy then " and is happy" else " and is not happy")


kidSpec :: SectionsSpec Text
kidSpec = section "name" "Name of the kid"

-- | Matches the 'yes' and 'no' atoms
yesOrNo :: ValueSpec Bool
yesOrNo = MapSpec (== Left()) $
          ChoiceSpec (AtomSpec (MkAtom "yes")) (AtomSpec (MkAtom "no"))

