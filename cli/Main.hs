module Main
where

import Text.Casing
import System.Environment
import System.Exit
import Control.Monad (forM_)
import Text.Printf (printf)

data Format
  = Snake
  | QuietSnake
  | ScreamingSnake
  | Camel
  | Pascal
  | Kebab
  | Words
  | Any
  deriving (Show, Eq, Enum, Bounded, Ord)

ppFormat :: Format -> String
ppFormat Snake = "snake"
ppFormat QuietSnake = "quiet-snake"
ppFormat ScreamingSnake = "screaming-snake"
ppFormat Camel = "camel"
ppFormat Pascal = "pascal"
ppFormat Kebab = "kebab"
ppFormat Words = "words"
ppFormat Any = "any"

descFormat :: Format -> String
descFormat Snake = "snake_Case (keep casing)"
descFormat QuietSnake = "quiet_snake (all lowercase)"
descFormat ScreamingSnake = "SCREAMING_SNAKE (all uppercase)"
descFormat Camel = "camelCase (initial lowercase)"
descFormat Pascal = "PascalCase (initial uppercase)"
descFormat Kebab = "kebab-case (all lowercase)"
descFormat Words = "print as Words (keep casing)"
descFormat Any = "any (parse as any; when writing, concatenate without separators, keep casing)"

parseFormat :: String -> Format
parseFormat "snake" = Snake
parseFormat "quiet-snake" = QuietSnake
parseFormat "qsnake" = QuietSnake
parseFormat "screaming-snake" = ScreamingSnake
parseFormat "ssnake" = ScreamingSnake
parseFormat "camel" = Camel
parseFormat "pascal" = Pascal
parseFormat "humps" = Camel
parseFormat "kebab" = Kebab
parseFormat "words" = Words
parseFormat "any" = Any
parseFormat "" = Any
parseFormat f = error $ "Unknown/unsupported format: " <> show f

from :: Format -> String -> Identifier String
from Any = fromAny
from Snake = fromSnake
from QuietSnake = fromSnake
from ScreamingSnake = fromSnake
from Camel = fromHumps
from Pascal = fromHumps
from Kebab = fromKebab
from Words = fromWords

to :: Format -> Identifier String -> String
to Any = concat . unIdentifier
to Snake = toSnake
to QuietSnake = toQuietSnake
to ScreamingSnake = toScreamingSnake
to Camel = toCamel
to Pascal = toPascal
to Kebab = toKebab
to Words = toWords

data Options =
  Options
    { optFromFormat :: Format
    , optToFormat :: Format
    , optFiles :: [FilePath]
    }

defOptions :: Options
defOptions =
  Options
    { optFromFormat = Any
    , optToFormat = Any
    , optFiles = []
    }

parseArgs :: [String] -> Options -> IO Options
parseArgs [] opts = return opts
parseArgs ("-h":_) opts = do
  printHelp
  exitSuccess
parseArgs ("--help":_) opts = do
  printHelp
  exitSuccess
parseArgs ("-f":fmt:xs) opts =
  parseArgs xs $
    opts { optFromFormat = parseFormat fmt }
parseArgs ["-f"] opts =
  return opts { optFromFormat = Any }
parseArgs ("-t":fmt:xs) opts =
  parseArgs xs $
    opts { optToFormat = parseFormat fmt }
parseArgs ["-t"] opts =
  return opts { optToFormat = Any }
parseArgs (('-':o):xs) opts =
  error $ "Unknown option -" ++ o
parseArgs (f:xs) opts =
  parseArgs xs $
    opts { optFiles = optFiles opts ++ [f] }

printHelp :: IO ()
printHelp = do
  putStrLn "casing - Convert between casing conventions"
  putStrLn ""
  putStrLn "Copyright (c) 2015,2020 Tobias Dammers"
  putStrLn "This is free software. See enclosed LICENSE file for details."
  putStrLn ""
  putStrLn "Usage:"
  putStrLn "casing {-f FROM_FMT} {-t TO_FMT} [FILES...]"
  putStrLn "casing {-h|--help}"
  putStrLn ""
  putStrLn "OPTIONS"
  putStrLn "-h, --help        Print this help text."
  putStrLn "-f FROM_FMT       Source casing convention. See below."
  putStrLn "-t TO_FMT         Target casing convention. See below."
  putStrLn ""
  putStrLn "FILES             One or more input files to process. If not files"
  putStrLn "                  are given, convert input from STDIN instead."
  putStrLn ""
  putStrLn "SUPPORTED FORMATS"
  forM_ [minBound..] $ \fmt ->
    printf "%-18s %s\n" (ppFormat fmt) (descFormat fmt)

processFile :: (String -> Identifier String) -> (Identifier String -> String) -> FilePath -> IO ()
processFile f t fn = do
  str <- readFile fn
  processString f t fn

processString :: (String -> Identifier String) -> (Identifier String -> String) -> String -> IO ()
processString f t str = do
  let strs = lines str
  forM_ strs $ putStrLn . t . f

main :: IO ()
main = do
  opts <- flip parseArgs defOptions =<< getArgs
  let t = to (optToFormat opts)
  let f = from (optFromFormat opts)
  case optFiles opts of
    [] -> getContents >>= processString f t
    xs -> forM_ xs $ processFile f t
