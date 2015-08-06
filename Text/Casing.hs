{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Text.Casing
( Identifier
, fromHumps, fromKebab, fromSnake, fromAny
, toCamel, toPascal, toSnake, toQuietSnake, toScreamingSnake, toKebab
, pascal, camel, snake, quietSnake, screamingSnake, kebab
)
where

import Data.Char
import Data.List (intersperse)
import Data.List.Split (wordsBy)
import Control.Applicative

newtype Identifier a = Identifier { unIdentifier :: [a] }
    deriving (Monad, Functor, Applicative, Show)

wordCase :: String -> String
wordCase "" = ""
wordCase (x:xs) = toUpper x : map toLower xs

fromHumps :: String -> Identifier String
fromHumps = Identifier . go
    where
        go "" = [""]
        go (x:[]) = [x:[]]
        go (x:y:xs)
            | (not $ isUpper x) && (isUpper y) =
                let (z:zs) = go (y:xs)
                in [x]:(z:zs)
            | otherwise =
                let (z:zs) = go (y:xs)
                in (x:z):zs

fromKebab :: String -> Identifier String
fromKebab = Identifier . wordsBy (== '-')

fromSnake :: String -> Identifier String
fromSnake = Identifier . wordsBy (== '_')

fromAny :: String -> Identifier String
fromAny str = fromHumps str >>= fromKebab >>= fromSnake

toPascal :: Identifier String -> String
toPascal = concat . map wordCase . unIdentifier

toCamel :: Identifier String -> String
toCamel (Identifier (x:xs)) = concat $ map toLower x:map wordCase xs

toKebab :: Identifier String -> String
toKebab = concat . intersperse "-" . map (map toLower) . unIdentifier

toSnake :: Identifier String -> String
toSnake = concat . intersperse "_" . unIdentifier

toQuietSnake :: Identifier String -> String
toQuietSnake = map toLower . toSnake

toScreamingSnake :: Identifier String -> String
toScreamingSnake = map toUpper . toSnake

pascal :: String -> String
pascal = toPascal . fromAny

camel :: String -> String
camel = toCamel . fromAny

snake :: String -> String
snake = toSnake . fromAny

quietSnake :: String -> String
quietSnake = toQuietSnake . fromAny

screamingSnake :: String -> String
screamingSnake = toScreamingSnake . fromAny

kebab :: String -> String
kebab = toKebab . fromAny
