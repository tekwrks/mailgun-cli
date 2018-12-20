module Environment
  ( Environment(..)
  , get
  ) where

import Paths_mailgun_cli (version)
import Data.Version (showVersion)

import System.Exit (exitSuccess)
import Control.Monad (when)
import System.Environment (getArgs)
import Mail.Hailgun (HailgunContext(..))
import Data.Maybe (fromMaybe, maybe)
import Data.List (nub)
import Control.Applicative ((<|>))

import qualified Errors (noTemplate, parseFailed)
import Flags (Flags, parse, usage)
import qualified Flags (Flag(Help, Version, Config, Mustache))
import Variables.Types (Variables)
import qualified Variables.Variables as Variables (get)
import Config.Types (Config)
import qualified Config.Config as Config (get, Config(..))
import qualified Context (create)
import Template (TemplateDesc(..), Template)
import qualified Template (get)

data Environment = Environment
  { flags :: Flags
  , variables :: Variables
  , context :: HailgunContext
  , template :: Template
  } deriving (Show)

get :: IO Environment
get = do
  (flags, args, mconfig) <- getFlags
  handleSpecials flags
  variables <- Variables.get args mconfig
  context <- Context.create flags mconfig
  template <- maybe Errors.noTemplate Template.get $ getTemplate flags mconfig
  return $ Environment flags variables context template

getTemplate :: Flags -> Maybe Config.Config -> Maybe TemplateDesc
getTemplate fs Nothing = templateFromFlags fs
getTemplate fs (Just c) = templateFromFlags fs <|> Config.template c

templateFromFlags :: Flags -> Maybe TemplateDesc
templateFromFlags [] = Nothing
templateFromFlags (Flags.Mustache p :fs) = Just $ TemplateDesc "mustache" p
templateFromFlags (f:fs) = templateFromFlags fs

getFlags :: IO (Flags, [String], Maybe Config)
getFlags = do
  (flags', args') <- parse =<< getArgs
  mConfig <- configFromFlags flags'
  let mflags'' = maybe Nothing Config.flags mConfig
  (flags'', args'') <- parse $ fromMaybe [] mflags''
  return (nub $ flags' ++ flags'', nub $ args' ++ args'', mConfig)

configFromFlags :: Flags -> IO (Maybe Config)
configFromFlags [] = return Nothing
configFromFlags (Flags.Config c :fs) = do
  eConfig <- Config.get c
  either Errors.parseFailed (return . Just) eConfig
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

