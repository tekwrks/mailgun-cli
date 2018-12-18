module Environment
  ( Environment(..)
  , get
  ) where

import Paths_mailgun_cli (version)
import Data.Version (showVersion)

import System.Exit
import Control.Monad (when)
import System.Environment (getArgs)
import Mail.Hailgun (HailgunContext(..))
import Data.Maybe (fromMaybe)
import Data.List (nub)

import Flags (Flags, parse, usage)
import qualified Flags (Flag(Help, Version, Config))
import Variables.Types (Variables)
import qualified Variables.Variables as Variables (get)
import Config.Types (Config)
import qualified Config.Config as Config (get, Config(..))

data Environment = Environment
  { flags :: Flags
  , variables :: Variables
  , context :: HailgunContext
  } deriving (Show)

get :: IO Environment
get = do
  (flags, args, mconfig) <- getFlags
  handleSpecials flags
  variables <- Variables.get args mconfig
  let context = HailgunContext "a" "b" Nothing
  return $ Environment flags variables context

getFlags :: IO (Flags, [String], Maybe Config)
getFlags = do
  (flags', args') <- parse =<< getArgs
  mConfig <- configFromFlags flags'
  let mflags'' = fromMaybe Nothing $ Config.flags <$> mConfig
  (flags'', args'') <- parse $ fromMaybe [] mflags''
  return (nub $ flags' ++ flags'', nub $ args' ++ args'', mConfig)

configFromFlags :: Flags -> IO (Maybe Config)
configFromFlags (Flags.Config c :fs) = do
  eConfig <- Config.get c
  either parseError (return . Just) eConfig
    where
      parseError e = do
        print e
        exitWith $ ExitFailure 2
configFromFlags (f:fs) = configFromFlags fs

handleSpecials :: Flags -> IO ()
handleSpecials fs = do
  when (Flags.Help `elem` fs) $ do
    when (Flags.Version `elem` fs) $ putStrLn $ showVersion version
    putStrLn usage
    exitSuccess
  when (Flags.Version `elem` fs) $ do
    putStrLn $ showVersion version
    exitSuccess

