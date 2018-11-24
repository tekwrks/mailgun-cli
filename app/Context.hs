module Context
  ( create
  ) where

import Data.Maybe
import Data.Either
import Control.Applicative ((<|>))
import System.IO
import System.Exit

import Mail.Hailgun (HailgunContext(..))
import Data.Yaml (ParseException)

import Flags (Flag(..), Flags)
import qualified Config (decodeContext)

data Context = Context
  { domain :: Maybe String
  , apiKey :: Maybe String
  } deriving (Eq, Show)

findDomain :: Flags -> Maybe String
findDomain [] = Nothing
findDomain (Domain d :fs) = Just d
findDomain (f:fs) = findDomain fs

findKey :: Flags -> Maybe String
findKey [] = Nothing
findKey (ApiKey k :fs) = Just k
findKey (f:fs) = findKey fs

fromArgs :: Flags -> Context
fromArgs fs = Context d k
  where
    d = findDomain fs
    k = findKey fs

fromFile :: Flags -> IO Context
fromFile [] = return $ Context Nothing Nothing
fromFile (Config config :fs) = do
  econtext <- Config.decodeContext config
  either fileError (fileParsed . flexible) econtext
  where
    fileError e = do
      hPutStrLn stderr $ "Error: " ++ show e
      exitWith $ ExitFailure 1
    fileParsed c = return $ Context (domain c) (apiKey c)
fromFile (f:fs) = fromFile fs

override :: Context -> Context -> Context
override base over =
  Context
    (domain over <|> domain base)
    (apiKey over <|> apiKey base)

flexible :: HailgunContext -> Context
flexible c = Context
  (Just $ hailgunDomain c)
  (Just $ hailgunApiKey c)

concrete :: Context -> IO HailgunContext
concrete Context{ domain=Just d, apiKey=Just k } =
  return $ HailgunContext d k Nothing
concrete _ = notEnoughContext

create :: Flags -> IO HailgunContext
create [] = notEnoughContext
create fs = do
  let cArgs = fromArgs fs
  cFile <- fromFile fs
  concrete $ override cFile cArgs

notEnoughContext = do
  hPutStrLn stderr "Error: Provide Domain and ApiKey"
  exitWith $ ExitFailure 1

