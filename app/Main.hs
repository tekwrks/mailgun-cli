module Main where

import Paths_mailgun_cli (version)
import Data.Version (showVersion)

import System.Exit
import Control.Monad (when)

import Environment (Environment(..))
import qualified Environment (get)

import qualified Flags (Flag(Help, Version))
import Flags (usage)
import qualified Variables.Variables as Variables (get)
import qualified Context.Context as Context (create)

main :: IO ()
main = do
  env <- Environment.get
  print env
  handleSpecial env
  variables <- Variables.get env
  print variables
  econtext <- Context.create env
  case econtext of
    Left err -> do
      print err
      exitWith $ ExitFailure 1
    Right context -> do
      print context

handleSpecial :: Environment -> IO ()
handleSpecial env = do
  let fs = flags env
  when (Flags.Help `elem` fs) $ do
    putStrLn usage
    exitSuccess
  when (Flags.Version `elem` fs) $ do
    putStrLn $ showVersion version
    exitSuccess
  return ()

