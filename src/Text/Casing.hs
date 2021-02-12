{-# LANGUAGE BangPatterns, DeriveTraversable, GeneralizedNewtypeDeriving #-}
-- | Conversions between several common identifier casing conventions:
--
-- - @PascalCase@ - no spacing between words, first letter in word is
-- uppercase, all others are lowercase.
-- - @camelCase@ - like @PascalCase@, but the very first letter is lowercase.
-- - @kebab-case@ - everything lowercase, dash delimits words.
-- - @snake_Case@ - underscores delimit words, case is unrestricted.
-- - @quiet_snake_case@ - underscores delimit words, everything lowercase.
-- - @SCREAMING_SNAKE_CASE@ - underscores delimit words, everything uppercase.
module Text.Casing
(
-- * Types
Identifier (..)
-- * Parsing
, fromHumps
, fromKebab
, fromSnake
, fromWords
, fromAny
-- * Generating
, toCamel
, toPascal
, toSnake
, toQuietSnake
, toScreamingSnake
, toKebab
, toWords
-- * Shorthand functions
, pascal
, camel
, snake
, quietSnake
, screamingSnake
, kebab
, wordify
-- * Miscellaneous
, dropPrefix
)
where

import Data.Char
import Data.Semigroup(Semigroup((<>)))
import Data.List (intercalate)
import Data.List.Split (wordsBy)
import Control.Applicative

-- | An opaque type that represents a parsed identifier.
newtype Identifier a = Identifier { unIdentifier :: [a] }
    deriving (Monad, Functor, Applicative, Show, Foldable, Traversable, Eq)

instance Semigroup (Identifier a) where
    Identifier a <> Identifier b = Identifier (a <> b)

wordCase :: String -> String
wordCase "" = ""
wordCase (x:xs) = toUpper x : map toLower xs

-- | Convert from "humped" casing (@camelCase@ or @PascalCase@)
fromHumps :: String -> Identifier String
fromHumps = Identifier . _fromHumps

_fromHumps :: String -> [String]
_fromHumps "" = [""]
_fromHumps c = _fromHumps' c

_unsnoc :: [a] -> a -> ([a], a)
_unsnoc [] x = ([], x)
_unsnoc (y:ys) x = let !(za, zb) = _unsnoc ys y in (x:za, zb)

_fromHumps' :: String -> [String]
_fromHumps' [] = []
_fromHumps' ca@(c:cs)
    | isUpper c = go (span isUpper cs)
    | otherwise = let (ba, bb) = break isUpper cs in (c:ba) : _fromHumps' bb
    where go (_,[]) = [ca]
          go (ls, rs) = addlsa ((l:y) : ys)
              where !(lsa, l) = _unsnoc ls c
                    !(y:ys) = _fromHumps' rs
                    addlsa | null lsa = id
                           | otherwise = (lsa :)

fromWords :: String -> Identifier String
fromWords = Identifier . words

-- | Convert from @kebab-cased-identifiers@
fromKebab :: String -> Identifier String
fromKebab = Identifier . wordsBy (== '-')

-- | Convert from @snake_cased@ (either flavor)
fromSnake :: String -> Identifier String
fromSnake = Identifier . wordsBy (== '_')

-- | Convert from anything, including mixed casing.
fromAny :: String -> Identifier String
fromAny str = fromHumps str >>= fromKebab >>= fromSnake >>= fromWords

-- | To @PascalCase@
toPascal :: Identifier String -> String
toPascal = concatMap wordCase . unIdentifier

-- | To @camelCase@
toCamel :: Identifier String -> String
toCamel (Identifier []) = ""
toCamel (Identifier (x:xs)) = map toLower x ++ concatMap wordCase xs

-- | To @kebab-case@
toKebab :: Identifier String -> String
toKebab = intercalate "-" . map (map toLower) . unIdentifier

-- | To @snake_Case@
toSnake :: Identifier String -> String
toSnake = intercalate "_" . unIdentifier

-- | To @quiet_snake_case@
toQuietSnake :: Identifier String -> String
toQuietSnake = map toLower . toSnake

-- | To @SCREAMING_SNAKE_CASE@
toScreamingSnake :: Identifier String -> String
toScreamingSnake = map toUpper . toSnake

-- | To @word Case@
toWords :: Identifier String -> String
toWords = unwords . unIdentifier

-- | Directly convert to @PascalCase@ through 'fromAny'
pascal :: String -> String
pascal = toPascal . fromAny

-- | Directly convert to @camelCase@ through 'fromAny'
camel :: String -> String
camel = toCamel . fromAny

-- | Directly convert to @snake_Case@ through 'fromAny'
snake :: String -> String
snake = toSnake . fromAny

-- | Directly convert to @quiet_snake_case@ through 'fromAny'
quietSnake :: String -> String
quietSnake = toQuietSnake . fromAny

-- | Directly convert to @SCREAMING_SNAKE_CASE@ through 'fromAny'
screamingSnake :: String -> String
screamingSnake = toScreamingSnake . fromAny

-- | Directly convert to @kebab-case@ through 'fromAny'
kebab :: String -> String
kebab = toKebab . fromAny

-- | Directly convert to @word Case@ through 'fromAny'
wordify :: String -> String
wordify = toWords . fromAny

-- | Drop the first word from a parsed identifier. Typical usage is between
-- parsing and writing, e.g.: @toKebab . dropPrefix . fromAny $ "strHelloWorld" == "hello-world"@
dropPrefix :: Identifier [a] -> Identifier [a]
dropPrefix = fmap (drop 1)
