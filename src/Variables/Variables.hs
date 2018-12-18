module Variables.Variables
  ( get
  , Variable
  , Variables
  ) where

import Data.List (nubBy, filter)
import System.IO
import System.Exit
import qualified Data.Text as T

import Flags (Flag(..), Flags)
import Config.Types (Config(variables))

import Variables.Types (Variable, Variables)

fromConfig :: Maybe Config -> Variables
fromConfig Nothing = []
fromConfig (Just c) = variables c

fromStrings :: [String] -> Variables
fromStrings [] = []
fromStrings (a:as) =
  let (k,v) = T.breakOn "=" (T.pack a)
   in (T.unpack k, T.unpack . T.tail $ v) : (fromStrings as)

get :: [String] -> Maybe Config -> IO Variables
get args mconfig = do
  let vArgs = fromStrings args
  let vConfig = fromConfig mconfig
  return . mergeFilter $ vArgs ++ vConfig
    where
      mergeFilter =
        nubBy (\a b -> fst a == fst b) .
        filter (\a -> snd a /= "")

