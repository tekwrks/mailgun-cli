module Main where

import System.Exit
import System.IO

import Environment (get, Environment(..))
import Template (Template)
import qualified Template (substitute, variables, render)

main :: IO ()
main = do
  env <- get
  -- print env
  let substituted = Template.substitute (template env) (variables env)
  print substituted
  rendered <- render substituted
  print rendered

render :: Template -> IO String
render t = do
  let unsubed = Template.variables t
  if unsubed == []
     then do
       maybe couldNotRender return $ Template.render t
     else do
       putStrLn "failed : no value for varaibles: "
       mapM (hPrint stderr) unsubed
       exitWith $ ExitFailure 4
  where
    couldNotRender = do
      putStrLn "failed: could not render template"
      exitWith $ ExitFailure 5

