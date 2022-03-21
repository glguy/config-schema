{-# Language OverloadedStrings #-}
{-|
Module      : Main
Description : Unit tests for config-schema
Copyright   : (c) Eric Mertens, 2017
License     : ISC
Maintainer  : emertens@gmail.com
-}
module Main (main) where

import           Config
import           Config.Schema
import           Control.Applicative
import           Data.Foldable
import           Data.Text (Text)
import qualified Data.Text as Text

-- tests that are expected to pass.
--
-- The input sources are a list of lists of lines. Each outer list
-- element contains a list of lines representing a complete input
-- source. Each of these variations must pass the test.
test ::
  Show a =>
  Eq   a =>
  ValueSpec a {- ^ specification to match -} ->
  a           {- ^ expected output        -} ->
  [[Text]]    {- ^ inputs sources         -} ->
  IO ()
test spec expected txtss =
  for_ txtss $ \txts ->
  case parse (Text.unlines txts) of
    Left e -> fail (show e)
    Right v ->
      case loadValue spec v of
        Left e -> fail (show e)
        Right x | x == expected -> return ()
                | otherwise -> fail ("Got " ++ show x ++ " but expected " ++
                                     show expected)

main :: IO ()
main = sequenceA_

  [ test anySpec ("Hello world"::Text)
    [["\"Hello world\""]
    ,["\"H\\101l\\&l\\o157 \\"
     ,"  \\w\\x6frld\""]
    ]

  , test anySpec (1234::Integer)
    [["1234"]
    ,["1234.0"]
    ]

  , test anySpec (0.65::Rational)
    [["0.65e0"]
    ,["65e-2"]
    ,["6.5e-1"]
    ,["0.65"]
    ]

  , test anyAtomSpec "default"
    [["default"]]

  , test (atomSpec "testing-1-2-3") ()
    [["testing-1-2-3"]]

  , test (textSpec' "specific-text") ()
    [["\"specific-text\""]]

  , test (listSpec anySpec) ([]::[Integer])
    [["[]"]
    ,["[ ]"]]

  , test (listSpec anyAtomSpec) ["ḿyatoḿ"]
    [["[ḿyatoḿ]"]
    ,[" [ ḿyatoḿ ] "]
    ,["* ḿyatoḿ"]
    ]

  , test anySpec [1,2,3::Int]
    [["[1,2,3]"]
    ,["[1,2,3,]"]
    ,["* 1"
     ,"* 2"
     ,"* 3"]
    ]

  , test (listSpec anySpec) [[1,2],[3,4::Int]]
    [["[[1,2,],[3,4]]"]
    ,["*[1,2]"
     ,"*[3,4]"]
    ,["**1"
     ," *2"
     ,"* *3"
     ,"  *4"
     ]
    ]

  , test (assocSpec anySpec) ([]::[(Text,Int)])
    [["{}"]
    ,["{ }"]
    ]

  , test (assocSpec anySpec) [("k1",10::Int), ("k2",20)]
    [["{k1: 10, k2: 20}"]
    ,["{k1: 10, k2: 20,}"]
    ,["k1 : 10"
     ,"k2: 20"]
    ]

  , test anySpec [ Left (1::Int), Right ("two"::Text) ]
    [["[1, \"two\"]"]
    ,["* 1"
     ,"* \"two\""]
    ]

  , test (sectionsSpec "test"
            (liftA2 (,) (reqSection "k1" "") (reqSection "k2" "")))
         (10 :: Int, 20 :: Int)
    [["k1: 10"
     ,"k2: 20"]
    ,["k2: 20"
     ,"k1: 10"]
    ]

  , test (sectionsSpec "test"
            (liftA2 (,) (optSection "k1" "") (reqSection "k2" "")))
         (Just 10 :: Maybe Int, 20 :: Int)
    [["k1: 10"
     ,"k2: 20"]
    ,["k2: 20"
     ,"k1: 10"]
    ]

  , test (sectionsSpec "test"
            (liftA2 (,) (optSection "k1" "") (reqSection "k2" "")))
         (Nothing :: Maybe Int, 20 :: Int)
    [["k2: 20"]
    ,["{k2: 20}"]
    ]

  -- This isn't a good idea, but it currently works
  , test (sectionsSpec "test"
            (liftA2 (,) (reqSection "k1" "") (reqSection "k1" "")))
         ("first"::Text, 50::Int)
    [["k1: \"first\""
     ,"k1: 50"]
    ]

  , test (sectionsSpec "test" (pure ())) ()
    [["{}"]
    ]
  ]
