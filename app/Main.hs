module Main where

import Paths_mailgun_cli (version)
import Data.Version (showVersion)

import System.Exit
import Control.Monad (when)

import Environment (Environment(..))
import qualified Environment (get)

import Control.Monad.Trans.Reader
import Control.Monad.IO.Class

import qualified Flags (Flag(Help, Version))
import Flags (usage)
import qualified Variables.Variables as Variables (get)
-- import qualified Context.Context as Context (create)
-- import qualified Template (get, variables)

main :: IO ()
main = do
  env <- Environment.get
  print env
  runReaderT handleSpecial env
  variables <- runReaderT Variables.get env
  print variables

  -- context <- Context.create flags
  -- print context
  -- template <- Template.get "template.mustache"
  -- print template
  -- print $ Template.variables template

handleSpecial :: ReaderT Environment IO ()
handleSpecial = do
  fs <- flags <$> ask
  when (Flags.Help `elem` fs) $ liftIO $ do
    putStrLn usage
    exitSuccess
  when (Flags.Version `elem` fs) $ liftIO $ do
    putStrLn $ showVersion version
    exitSuccess
  return ()

