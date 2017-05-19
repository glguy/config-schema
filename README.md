config-schema
=============

[![Hackage](https://img.shields.io/hackage/v/config-schema.svg)](https://hackage.haskell.org/package/config-schema) [![Build Status](https://secure.travis-ci.org/glguy/config-schema.png?branch=master)](http://travis-ci.org/glguy/config-schema)

This package allows the user to define configuration schemas suitable for
matching against configuration files written in the
[config-value](https://hackage.haskell.org/package/config-value) format.
These schemas allow the user to extract an arbitrary Haskell value from
an interpretation of a configuration file. It also allows the user to
programatically generate documentation for the configuration files
accepted by the loader.

```haskell
{-# Language OverloadedStrings, ApplicativeDo #-}
module Example where

import qualified Data.Text as Text
import           Data.Text (Text)
import           Data.Monoid ((<>))
import           Data.Functor.Alt ((<!>))
import           Data.List.NonEmpty (NonEmpty)

import           Config
import           Config.Schema

exampleFile :: Text
exampleFile =
  " name: \"Johny Appleseed\" \n\
  \ age : 99                  \n\
  \ happy: yes                \n\
  \ kids:                     \n\
  \   * name: \"Bob\"         \n\
  \   * name: \"Tom\"         \n"

exampleValue :: Value Position
Right exampleValue = parse exampleFile

exampleSpec :: ValueSpecs Text
exampleSpec = sectionsSpec "" $
  do name  <- reqSection  "name" "Full name"
     age   <- reqSection  "age"  "Age of user"
     happy <- optSection' "happy" yesOrNo
              "Current happiness status"
     kids  <- reqSection' "kids"  (oneOrList kidSpec)
              "All children's names"

     return $
       let happyText = case happy of Just True  -> " and is happy"
                                     Just False -> " and is not happy"
                                     Nothing    -> " and is private"

       in name <> " is " <> Text.pack (show (age::Integer)) <>
             " years old and has kids " <>
             Text.intercalate ", " kids <>
             happyText

kidSpec :: ValueSpecs Text
kidSpec = sectionsSpec "kid" (reqSection "name" "Kid's name")


-- | Matches the 'yes' and 'no' atoms
yesOrNo :: ValueSpecs Bool
yesOrNo = True  <$ atomSpec "yes" <!>
          False <$ atomSpec "no"


printDoc :: IO ()
printDoc = print (generateDocs exampleSpec)
-- *Example> printDoc
-- Top-level configuration file fields:
--     name: REQUIRED text
--        Full name
--     age: REQUIRED integer
--        Age of user
--     happy: `yes` or `no`
--        Current happiness status
--     kids: REQUIRED kid or list of kid
--        All children
--
-- kid
--     name: REQUIRED text
--        Kid's name

example :: Either (NonEmpty (LoadError Position)) Text
example = loadValue exampleSpec exampleValue
-- *Example> exampleVal
-- Right "Johny Appleseed is 99 years old and has kids Bob, Tom and is happy"
```
