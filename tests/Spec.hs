module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Text.Casing

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "tests"
  [ testGroup "parsing"
    [ testGroup "fromHumps"
      [ testCase "no splits 1" $ do
          assertEqual ""
            (Identifier ["hello"])
            (fromHumps "hello")
      , testCase "no splits 2" $ do
          assertEqual ""
            (Identifier ["Hello"])
            (fromHumps "Hello")
      , testCase "non-alpha, no split" $ do
          assertEqual ""
            (Identifier ["-hello"])
            (fromHumps "-hello")
      , testCase "non-alpha, split" $ do
          assertEqual ""
            (Identifier ["-hello-", "World"])
            (fromHumps "-hello-World")
      , testCase "simple split camel" $ do
          assertEqual ""
            (Identifier ["hello", "World"])
            (fromHumps "helloWorld")
      , testCase "simple split pascal" $ do
          assertEqual ""
            (Identifier ["Hello", "World"])
            (fromHumps "HelloWorld")
      , testCase "single letter abbrev split" $ do
          assertEqual ""
            (Identifier ["Hello", "A", "World"])
            (fromHumps "HelloAWorld")
      , testCase "multi letter abbrev split" $ do
          assertEqual ""
            (Identifier ["Hello", "X", "M", "L", "World"])
            (fromHumps "HelloXMLWorld")
      , testCase "single letter upper" $ do
          assertEqual ""
            (Identifier ["A"])
            (fromHumps "A")
      , testCase "single letter lower" $ do
          assertEqual ""
            (Identifier ["a"])
            (fromHumps "a")
      ]
    , testGroup "fromKebab"
      [ testCase "no splits" $ do
          assertEqual ""
            (Identifier ["hello"])
            (fromKebab "hello")
      , testCase "single split" $ do
          assertEqual ""
            (Identifier ["hello", "world"])
            (fromKebab "hello-world")
      , testCase "leading split" $ do
          assertEqual ""
            (Identifier ["world"])
            (fromKebab "-world")
      , testCase "trailing split" $ do
          assertEqual ""
            (Identifier ["world"])
            (fromKebab "world-")
      , testCase "multiple dashes" $ do
          assertEqual ""
            (Identifier ["hello", "world"])
            (fromKebab "hello---world")
      ]
    , testGroup "fromSnake"
      [ testCase "no splits" $ do
          assertEqual ""
            (Identifier ["hello"])
            (fromSnake "hello")
      , testCase "single split" $ do
          assertEqual ""
            (Identifier ["hello", "world"])
            (fromSnake "hello_world")
      , testCase "leading split" $ do
          assertEqual ""
            (Identifier ["world"])
            (fromSnake "_world")
      , testCase "trailing split" $ do
          assertEqual ""
            (Identifier ["world"])
            (fromSnake "world_")
      , testCase "multiple dashes" $ do
          assertEqual ""
            (Identifier ["hello", "world"])
            (fromSnake "hello___world")
      ]
    , testGroup "fromWords"
      [ testCase "no splits" $ do
          assertEqual ""
            (Identifier ["hello"])
            (fromWords "hello")
      , testCase "single split" $ do
          assertEqual ""
            (Identifier ["hello", "world"])
            (fromWords "hello world")
      , testCase "leading split" $ do
          assertEqual ""
            (Identifier ["world"])
            (fromWords " world")
      , testCase "trailing split" $ do
          assertEqual ""
            (Identifier ["world"])
            (fromWords "world ")
      , testCase "multiple dashes" $ do
          assertEqual ""
            (Identifier ["hello", "world"])
            (fromWords "hello   world")
      ]
    , testGroup "fromAny"
      [ testCase "no splits 1" $ do
          assertEqual ""
            (Identifier ["hello"])
            (fromAny "hello")
      , testCase "no splits 2" $ do
          assertEqual ""
            (Identifier ["Hello"])
            (fromAny "Hello")
      , testCase "simple split camel" $ do
          assertEqual ""
            (Identifier ["hello", "World"])
            (fromAny "helloWorld")
      , testCase "simple split pascal" $ do
          assertEqual ""
            (Identifier ["Hello", "World"])
            (fromAny "HelloWorld")
      , testCase "single letter abbrev split" $ do
          assertEqual ""
            (Identifier ["Hello", "A", "World"])
            (fromAny "HelloAWorld")
      , testCase "multi letter abbrev split" $ do
          assertEqual ""
            (Identifier ["Hello", "X", "M", "L", "World"])
            (fromAny "HelloXMLWorld")
      , testCase "single letter upper" $ do
          assertEqual ""
            (Identifier ["A"])
            (fromAny "A")
      , testCase "single letter lower" $ do
          assertEqual ""
            (Identifier ["a"])
            (fromAny "a")
      , testCase "no splits" $ do
          assertEqual ""
            (Identifier ["hello"])
            (fromAny "hello")
      , testCase "single split" $ do
          assertEqual ""
            (Identifier ["hello", "world"])
            (fromAny "hello-world")
      , testCase "leading split" $ do
          assertEqual ""
            (Identifier ["world"])
            (fromAny "-world")
      , testCase "trailing split" $ do
          assertEqual ""
            (Identifier ["world"])
            (fromAny "world-")
      , testCase "multiple dashes" $ do
          assertEqual ""
            (Identifier ["hello", "world"])
            (fromAny "hello---world")
      , testCase "no splits" $ do
          assertEqual ""
            (Identifier ["hello"])
            (fromAny "hello")
      , testCase "single split" $ do
          assertEqual ""
            (Identifier ["hello", "world"])
            (fromAny "hello_world")
      , testCase "leading split" $ do
          assertEqual ""
            (Identifier ["world"])
            (fromAny "_world")
      , testCase "trailing split" $ do
          assertEqual ""
            (Identifier ["world"])
            (fromAny "world_")
      , testCase "multiple dashes" $ do
          assertEqual ""
            (Identifier ["hello", "world"])
            (fromAny "hello___world")
      , testCase "no splits" $ do
          assertEqual ""
            (Identifier ["hello"])
            (fromAny "hello")
      , testCase "single split" $ do
          assertEqual ""
            (Identifier ["hello", "world"])
            (fromAny "hello world")
      , testCase "leading split" $ do
          assertEqual ""
            (Identifier ["world"])
            (fromAny " world")
      , testCase "trailing split" $ do
          assertEqual ""
            (Identifier ["world"])
            (fromAny "world ")
      , testCase "multiple dashes" $ do
          assertEqual ""
            (Identifier ["hello", "world"])
            (fromAny "hello   world")
      ]
    ]
  , testGroup "writing"
    [ testGroup "toPascal"
      [ testCase "toPascal simple" $ do
          assertEqual ""
            "HelloWorld"
            (toPascal $ Identifier ["hello", "world"])
      ]
    , testGroup "toCamel"
      [ testCase "toCamel simple" $ do
          assertEqual ""
            "helloWorld"
            (toCamel $ Identifier ["hello", "world"])
      ]
    , testGroup "toKebab"
      [ testCase "toKebab simple" $ do
          assertEqual ""
            "hello-world"
            (toKebab $ Identifier ["hello", "world"])
      ]
    , testGroup "toSnake"
      [ testCase "toSnake simple" $ do
          assertEqual ""
            "hello_world"
            (toSnake $ Identifier ["hello", "world"])
      , testCase "toScreamingSnake simple" $ do
          assertEqual ""
            "HELLO_WORLD"
            (toScreamingSnake $ Identifier ["hello", "world"])
      , testCase "toQuietSnake simple" $ do
          assertEqual ""
            "hello_world"
            (toQuietSnake $ Identifier ["Hello", "World"])
      ]
    ]
  ]
