module Environment
  ( Environment(..)
  , get
  , Message(..)
  ) where

import Paths_mailgun_cli (version)
import Data.Version (showVersion)

import System.Exit (exitSuccess)
import Control.Monad (when)
import System.Environment (getArgs)
import Mail.Hailgun (HailgunContext(..))
import Data.Maybe (fromMaybe, maybe, isNothing)
import Data.List (nub)
import Control.Applicative ((<|>))

import qualified Errors (noPlain, parseFailed)
import Flags (Flags, parse, usage)
import qualified Flags (Flag(Help, Version, Config, Plain, Html))
import Variables.Types (Variables)
import qualified Variables.Variables as Variables (get)
import Config.Types (Config)
import qualified Config.Config as Config (get, Config(..))
import qualified Context (create)
import Template (TemplateDesc(..), Template)
import qualified Template (get)

data Message = Message
  { plain :: Template
  , html :: Maybe Template
  } deriving (Show)

data Environment = Environment
  { flags :: Flags
  , variables :: Variables
  , context :: HailgunContext
  , message :: Message
  } deriving (Show)

get :: IO Environment
get = do
  (flags, args, mconfig) <- getFlags
  handleSpecials flags
  variables <- Variables.get args mconfig
  context <- Context.create flags mconfig
  plain <- maybe Errors.noPlain Template.get $ getPlain flags mconfig
  let htmlDesc = getHtml flags mconfig
  html <- case htmlDesc of
            Nothing -> return Nothing
            (Just d) -> Just <$> Template.get d
  let message = Message plain html
  return $ Environment flags variables context message

getPlain :: Flags -> Maybe Config.Config -> Maybe TemplateDesc
getPlain fs Nothing = plainFromFlags fs
getPlain fs (Just c) = plainFromFlags fs <|> Config.plain c

plainFromFlags :: Flags -> Maybe TemplateDesc
plainFromFlags [] = Nothing
plainFromFlags (Flags.Plain p :fs) = Just $ TemplateDesc "mustache" p
plainFromFlags (f:fs) = plainFromFlags fs

getHtml :: Flags -> Maybe Config.Config -> Maybe TemplateDesc
getHtml fs Nothing = htmlFromFlags fs
getHtml fs (Just c) = htmlFromFlags fs <|> Config.html c

htmlFromFlags :: Flags -> Maybe TemplateDesc
htmlFromFlags [] = Nothing
htmlFromFlags (Flags.Html p :fs) = Just $ TemplateDesc "mustache" p
htmlFromFlags (f:fs) = htmlFromFlags fs

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

