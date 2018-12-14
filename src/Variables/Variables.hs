module Variables.Variables
  ( get
  , Variable
  , Variables
  ) where

import Data.Either
import Data.Maybe
import System.IO
import System.Exit
import Data.Yaml (ParseException)

import Flags (Flag(..), Flags)
import Config (getVariables)
import Variables.Types (Variable, Variables)

fromFile :: Flags -> IO Variables
fromFile [] = return []
fromFile (Config config :fs) = do
  econtext <- Config.getVariables config
  either fileError fileParsed econtext
  where
    fileError e = do
      hPutStrLn stderr $ "Error: " ++ show e
      exitWith $ ExitFailure 1
    fileParsed = return
fromFile (f:fs) = fromFile fs

get :: Flags -> IO Variables
get [] = return []
get fs = do
  vFile <- fromFile fs
  return vFile

