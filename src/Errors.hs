module Errors where

import System.Exit
import System.IO

ioError :: IOError -> IO String
ioError e = do
  print e
  exitWith $ ExitFailure 0

getFlagsFailed :: (Show a) => a -> String -> IO b
getFlagsFailed m usage = do
  print m
  print usage
  -- hPutStrLn stderr usage
  exitWith $ ExitFailure 1

parseFailed :: (Show a) => a -> IO b
parseFailed e = do
  print e
  exitWith $ ExitFailure 2

noPlain :: IO a
noPlain = do
  putStrLn "failed : no plain text template found"
  exitWith $ ExitFailure 3

noValueForVariables :: [String] -> IO String
noValueForVariables vs = do
  putStrLn "failed : no value for variables: "
  mapM (hPrint stderr) vs
  exitWith $ ExitFailure 4

couldNotRender :: IO String
couldNotRender = do
  putStrLn "failed : could not render template"
  exitWith $ ExitFailure 5

notEnoughContext :: IO a
notEnoughContext = do
  print "failed : Domain and ApiKey are required, but found none"
  exitWith $ ExitFailure 6

parseMustache :: (Show a) => a -> IO b
parseMustache e = do
  print e
  exitWith $ ExitFailure 7

