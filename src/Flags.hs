module Flags
  ( Flag (..)
  , Flags
  , parse
  , usage
  ) where

import Data.Char
import Data.List
import Data.Maybe
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
import Text.Printf

usage :: String
usage = usageInfo header options
  where
    header = "Usage: mailgun-cli [OPTION ...]"

data Flag
  = Domain String
  | ApiKey String
  | Config String
  | NonInteractive
  | DryRun
  | Help
  | Version
  deriving (Eq, Show)

type Flags = [Flag]

options =
  [ Option []    ["domain"]         (ReqArg Domain "DOMAIN")       "Mailgun DOMAIN"
  , Option []    ["api-key"]        (ReqArg ApiKey "API-KEY")      "Mailgun API-KEY"
  , Option ['c'] ["config"]         (OptArg configp "config.yaml") "yaml config file"
  , Option ['n'] ["no-interaction"] (NoArg NonInteractive)         "non-interactive mode - fail instead of asking for user input/confirmation"
  , Option ['d'] ["dry-run"]        (NoArg DryRun)                 "dry-run, print actions, but don't execute anything"
  , Option ['h'] ["help"]           (NoArg Help)                   "Print this help message."
  , Option []    ["version"]        (NoArg Version)                "Print version."
  ]

configp :: Maybe String -> Flag
configp = Config . fromMaybe "config.yaml"

parse :: [String] -> IO (Flags, [String])
parse argv =
  case getOpt Permute options argv of
    (flags, args, []) ->
      return (nub flags, nub args)
    (_, _, errs) -> do
      hPutStrLn stderr (concat errs ++ usage)
      exitWith $ ExitFailure 1

