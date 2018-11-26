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
import Control.Monad (when)

version :: String
version = "Version: 0.1.0.0"

usage :: String
usage = usageInfo header options
  where
    header = "Usage: mailgun-cli [OPTION ...]"

data Flag
  = Domain String
  | ApiKey String
  | Config String
  | Help
  | Version
  deriving (Eq, Ord, Show)

type Flags = [Flag]

options =
  [ Option []    ["domain"]  (ReqArg Domain "DOMAIN")       "Mailgun DOMAIN"
  , Option []    ["api-key"] (ReqArg ApiKey "API-KEY")      "Mailgun API-KEY"
  , Option ['c'] ["config"]  (OptArg configp "config.yaml") "yaml config file"
  , Option ['h'] ["help"]    (NoArg Help)                   "Print this help message."
  , Option []    ["version"] (NoArg Version)                "Print version."
  ]

configp :: Maybe String -> Flag
configp = Config . fromMaybe "config.yaml"

parse :: [String] -> IO Flags
parse argv =
  case getOpt Permute options argv of
    (flags, _, []) ->
      if Help `elem` flags
        then do
          when (Version `elem` flags) $ putStrLn version
          hPutStrLn stderr usage
          exitSuccess
        else do
          when (Version `elem` flags) $ do { putStrLn version; exitSuccess; }
          return $ nub flags
    (_, _, errs) -> do
      hPutStrLn stderr (concat errs ++ usage)
      exitWith $ ExitFailure 1

