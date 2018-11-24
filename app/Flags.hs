module Flags
  ( Flag (..)
  , Flags
  , parse
  ) where

import Data.Char
import Data.List
import Data.Maybe
import System.Console.GetOpt
import System.Exit
import System.Environment
import System.IO
import Text.Printf

data Flag
  = Domain String
  | ApiKey String
  | Config String
  | Help
  deriving (Eq, Ord, Show)

type Flags = [Flag]

options =
  [ Option []    ["domain"]  (ReqArg Domain "DOMAIN")       "Mailgun DOMAIN"
  , Option []    ["api-key"] (ReqArg ApiKey "API-KEY")      "Mailgun API-KEY"
  , Option ['c'] ["config"]  (OptArg configp "config.yaml") "yaml config file"
  , Option ['h'] ["help"]    (NoArg Help)                   "Print this help message."
  ]

configp :: Maybe String -> Flag
configp = Config . fromMaybe "config.yaml"

parse :: [String] -> IO Flags
parse argv =
  case getOpt Permute options argv of
    (flags, _, []) ->
      if Help `elem` flags
        then do
          hPutStrLn stderr (usageInfo header options)
          exitSuccess
        else
          return $ nub flags
    (_, _, errs) -> do
      hPutStrLn stderr (concat errs ++ usageInfo header options)
      exitWith $ ExitFailure 1
  where
    header = "Usage: mailgun-cli [OPTION ...]"

