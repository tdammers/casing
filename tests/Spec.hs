module Main where

import Data.Char(isUpper)
-- import Test.QuickCheck.Property
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Text.Casing

main :: IO ()
main = defaultMain tests

-- Test if the new "fromHumps" is equivalent to the old one.
oldFromHumps :: String -> Identifier String
oldFromHumps = Identifier . go
    where
        go "" = [""]
        go (x:[]) = [x:[]]
        go xxs@(x:xs)
          | isUpper x =
              let lhs = takeWhile isUpper xxs
                  rhs = dropWhile isUpper xxs
              in
              if null rhs then
                [lhs]
              else
                let curLen = length lhs - 1
                    cur = take curLen lhs
                    rec = go rhs
                    nxt = drop curLen lhs ++ concat (take 1 rec)
                    rem = drop 1 rec
                    curL = if null cur then [] else [cur]
                    nxtL = if null nxt then [] else [nxt]
                in curL ++ nxtL ++ rem

          | otherwise =
              let cur = takeWhile (not . isUpper) xxs
                  rem = dropWhile (not . isUpper) xxs
              in
              if null rem then
                [cur]
              else
                cur:go rem

tests :: TestTree
tests = testGroup "tests"
  [ testGroup "parsing"
    [ testGroup "fromHumps"
      [ testCase "no splits 1" $
          assertEqual ""
            (Identifier ["hello"])
            (fromHumps "hello")
      , testCase "no splits 2" $
          assertEqual ""
            (Identifier ["Hello"])
            (fromHumps "Hello")
      , testCase "non-alpha, no split" $
          assertEqual ""
            (Identifier ["-hello"])
            (fromHumps "-hello")
      , testCase "non-alpha, split" $
          assertEqual ""
            (Identifier ["-hello-", "World"])
            (fromHumps "-hello-World")
      , testCase "simple split camel" $
          assertEqual ""
            (Identifier ["hello", "World"])
            (fromHumps "helloWorld")
      , testCase "simple split pascal" $
          assertEqual ""
            (Identifier ["Hello", "World"])
            (fromHumps "HelloWorld")
      , testCase "single letter abbrev split" $
          assertEqual ""
            (Identifier ["Hello", "A", "World"])
            (fromHumps "HelloAWorld")
      , testCase "multi letter abbrev split" $
          assertEqual ""
            (Identifier ["Hello", "XML", "World"])
            (fromHumps "HelloXMLWorld")
      , testCase "multi letter acronym at the end" $
          assertEqual ""
            (Identifier ["Hello", "XML"])
            (fromHumps "HelloXML")
      , testCase "single letter upper" $
          assertEqual ""
            (Identifier ["A"])
            (fromHumps "A")
      , testCase "single letter lower" $
          assertEqual ""
            (Identifier ["a"])
            (fromHumps "a")
      ]
    , testGroup "fromKebab"
      [ testCase "no splits" $
          assertEqual ""
            (Identifier ["hello"])
            (fromKebab "hello")
      , testCase "single split" $
          assertEqual ""
            (Identifier ["hello", "world"])
            (fromKebab "hello-world")
      , testCase "leading split" $
          assertEqual ""
            (Identifier ["world"])
            (fromKebab "-world")
      , testCase "trailing split" $
          assertEqual ""
            (Identifier ["world"])
            (fromKebab "world-")
      , testCase "multiple dashes" $
          assertEqual ""
            (Identifier ["hello", "world"])
            (fromKebab "hello---world")
      ]
    , testGroup "fromSnake"
      [ testCase "no splits" $
          assertEqual ""
            (Identifier ["hello"])
            (fromSnake "hello")
      , testCase "single split" $
          assertEqual ""
            (Identifier ["hello", "world"])
            (fromSnake "hello_world")
      , testCase "leading split" $
          assertEqual ""
            (Identifier ["world"])
            (fromSnake "_world")
      , testCase "trailing split" $
          assertEqual ""
            (Identifier ["world"])
            (fromSnake "world_")
      , testCase "multiple dashes" $
          assertEqual ""
            (Identifier ["hello", "world"])
            (fromSnake "hello___world")
      ]
    , testGroup "fromWords"
      [ testCase "no splits" $
          assertEqual ""
            (Identifier ["hello"])
            (fromWords "hello")
      , testCase "single split" $
          assertEqual ""
            (Identifier ["hello", "world"])
            (fromWords "hello world")
      , testCase "leading split" $
          assertEqual ""
            (Identifier ["world"])
            (fromWords " world")
      , testCase "trailing split" $
          assertEqual ""
            (Identifier ["world"])
            (fromWords "world ")
      , testCase "multiple dashes" $
          assertEqual ""
            (Identifier ["hello", "world"])
            (fromWords "hello   world")
      ]
    , testGroup "fromAny"
      [ testCase "no splits 1" $
          assertEqual ""
            (Identifier ["hello"])
            (fromAny "hello")
      , testCase "no splits 2" $
          assertEqual ""
            (Identifier ["Hello"])
            (fromAny "Hello")
      , testCase "simple split camel" $
          assertEqual ""
            (Identifier ["hello", "World"])
            (fromAny "helloWorld")
      , testCase "simple split pascal" $
          assertEqual ""
            (Identifier ["Hello", "World"])
            (fromAny "HelloWorld")
      , testCase "single letter abbrev split" $
          assertEqual ""
            (Identifier ["Hello", "A", "World"])
            (fromAny "HelloAWorld")
      , testCase "multi letter abbrev split" $
          assertEqual ""
            (Identifier ["Hello", "XML", "World"])
            (fromAny "HelloXMLWorld")
      , testCase "single letter upper" $
          assertEqual ""
            (Identifier ["A"])
            (fromAny "A")
      , testCase "single letter lower" $
          assertEqual ""
            (Identifier ["a"])
            (fromAny "a")
      , testCase "no splits" $
          assertEqual ""
            (Identifier ["hello"])
            (fromAny "hello")
      , testCase "single split" $
          assertEqual ""
            (Identifier ["hello", "world"])
            (fromAny "hello-world")
      , testCase "leading split" $
          assertEqual ""
            (Identifier ["world"])
            (fromAny "-world")
      , testCase "trailing split" $
          assertEqual ""
            (Identifier ["world"])
            (fromAny "world-")
      , testCase "multiple dashes" $
          assertEqual ""
            (Identifier ["hello", "world"])
            (fromAny "hello---world")
      , testCase "no splits" $
          assertEqual ""
            (Identifier ["hello"])
            (fromAny "hello")
      , testCase "single split" $
          assertEqual ""
            (Identifier ["hello", "world"])
            (fromAny "hello_world")
      , testCase "leading split" $
          assertEqual ""
            (Identifier ["world"])
            (fromAny "_world")
      , testCase "trailing split" $
          assertEqual ""
            (Identifier ["world"])
            (fromAny "world_")
      , testCase "multiple dashes" $
          assertEqual ""
            (Identifier ["hello", "world"])
            (fromAny "hello___world")
      , testCase "no splits" $
          assertEqual ""
            (Identifier ["hello"])
            (fromAny "hello")
      , testCase "single split" $
          assertEqual ""
            (Identifier ["hello", "world"])
            (fromAny "hello world")
      , testCase "leading split" $
          assertEqual ""
            (Identifier ["world"])
            (fromAny " world")
      , testCase "trailing split" $
          assertEqual ""
            (Identifier ["world"])
            (fromAny "world ")
      , testCase "multiple dashes" $
          assertEqual ""
            (Identifier ["hello", "world"])
            (fromAny "hello   world")
      ]
    ]
  , testGroup "writing"
    [ testGroup "toPascal"
      [ testCase "toPascal simple" $
          assertEqual ""
            "HelloWorld"
            (toPascal $ Identifier ["hello", "world"])
      ]
    , testGroup "toCamel"
      [ testCase "toCamel simple" $
          assertEqual ""
            "helloWorld"
            (toCamel $ Identifier ["hello", "world"])
      , testCase "toCamel empty" $
          assertEqual ""
            ""
            (toCamel $ Identifier [])
      ]
    , testGroup "toKebab"
      [ testCase "toKebab simple" $
          assertEqual ""
            "hello-world"
            (toKebab $ Identifier ["hello", "world"])
      ]
    , testGroup "toSnake"
      [ testCase "toSnake simple" $
          assertEqual ""
            "hello_world"
            (toSnake $ Identifier ["hello", "world"])
      , testCase "toScreamingSnake simple" $
          assertEqual ""
            "HELLO_WORLD"
            (toScreamingSnake $ Identifier ["hello", "world"])
      , testCase "toQuietSnake simple" $
          assertEqual ""
            "hello_world"
            (toQuietSnake $ Identifier ["Hello", "World"])
      ]
      , testProperty "simplification fromHumps is the same" (\c -> fromHumps c == oldFromHumps c)
    ]
  ]
