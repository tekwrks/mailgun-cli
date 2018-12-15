module Environment
  ( Environment(..)
  , get
  ) where

import System.Environment (getArgs)
import Flags (Flags, parse)

data Environment = Environment
  { flags :: Flags
  , args :: [String]
  } deriving (Show, Eq)

get :: IO (Environment)
get = do
  (flags, args) <- parse =<< getArgs
  let env = Environment flags args
  return env

