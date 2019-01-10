module Main where

import System.Exit
import Mail.Hailgun (HailgunSendResponse(..), HailgunErrorResponse(..))

import Environment (get, Environment(..))
import qualified Message (render, substitute)
import qualified Send (send)

main :: IO ()
main = do
  env <- get
  print env
  content <- Message.render $
    Message.substitute (message env) (variables env)
  res <- Send.send (context env) content (header env)
  printResult res
  exitSuccess

printResult :: Either HailgunErrorResponse HailgunSendResponse -> IO ()
printResult (Left e) = do
  print $ herMessage e
  exitWith $ ExitFailure 0
printResult (Right r) = do
  print $ hsrMessage r
  print $ hsrId r

