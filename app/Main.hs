module Main where

import System.Environment (getArgs)

import Mail.Hailgun
import Data.Yaml

import qualified Flags (parse)
import Flags (Flag(..), Flags)
import qualified Config (decodeContext)

createContext :: Flags -> IO HailgunContext
createContext fs = do
  return $ HailgunContext "domain" "key" Nothing

main :: IO ()
main = do
  flags <- Flags.parse =<< getArgs
  print flags
  context <- createContext flags
  print context
  contextFromFile <- Config.decodeContext "./config.yaml"
  print contextFromFile
