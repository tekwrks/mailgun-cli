module Flags
  ( Flag (..)
  , Flags
  , parse
  , usage
  ) where

import Data.List
import Data.Maybe
import System.Console.GetOpt

import qualified Errors (getFlagsFailed)

usage :: String
usage = usageInfo header options
  where
    header = "Usage: mailgun-cli [OPTION ...]"

data Flag
  = Domain String
  | ApiKey String
  | Config String
  | Plain FilePath
  | Html FilePath
  | DryRun
  | Help
  | Version
  deriving (Eq, Show)

type Flags = [Flag]

options =
  [ Option []    ["domain"]         (ReqArg Domain "DOMAIN")         "Mailgun DOMAIN"
  , Option []    ["api-key"]        (ReqArg ApiKey "API-KEY")        "Mailgun API-KEY"
  , Option ['c'] ["config"]         (OptArg configp "config.yaml")   "yaml config file"
  , Option []    ["plain"]          (OptArg plainp "plain.mustache") "plain text mustache template"
  , Option []    ["html"]           (OptArg htmlp "html.mustache")   "plain text mustache template"
  , Option ['d'] ["dry-run"]        (NoArg DryRun)                   "dry-run, print actions, but don't execute anything"
  , Option ['h'] ["help"]           (NoArg Help)                     "Print this help message."
  , Option []    ["version"]        (NoArg Version)                  "Print version."
  ]

configp :: Maybe String -> Flag
configp = Config . fromMaybe "config.yaml"

plainp :: Maybe String -> Flag
plainp = Plain . fromMaybe "plain.mustache"

htmlp :: Maybe String -> Flag
htmlp = Html . fromMaybe "html.mustache"

parse :: [String] -> IO (Flags, [String])
parse argv =
  case getOpt Permute options argv of
    (flags, args, []) -> return (nub flags, nub args)
    (_, _, errs) -> Errors.getFlagsFailed (concat errs) usage

