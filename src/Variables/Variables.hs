module Variables.Variables
  ( get
  , Variable
  , Variables
  ) where

import Data.Either
import Data.Maybe
import Data.List (nubBy, filter)
import System.IO
import System.Exit
import Data.Yaml (ParseException)
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import qualified Data.Text as T

import Environment (Environment(..))
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

fromArgs :: [String] -> IO Variables
fromArgs [] = return []
fromArgs (a:as) = do
  rest <- fromArgs(as)
  let (k,v) = T.breakOn "=" (T.pack a)
  return $ (T.unpack k, T.unpack . T.tail $ v) : rest

get :: ReaderT Environment IO Variables
get = do
  fs <- flags <$> ask
  as <- args <$> ask
  vFile <- liftIO $ fromFile fs
  vArgs <- liftIO $ fromArgs as
  return . mergeFilter $ vArgs ++ vFile
  where
    mergeFilter =
      nubBy (\a b -> (fst a) == (fst b)) .
      filter (\a -> (snd a) /= "")

