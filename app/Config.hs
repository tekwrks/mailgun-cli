module Config
  ( decodeContext
  ) where

import System.IO
import Data.Either

import Mail.Hailgun
import Data.Yaml
import Data.Aeson.Types (typeMismatch)

data Config = Config
  { domain :: String
  , apiKey :: String
  }

instance FromJSON Config where
  parseJSON (Object v) = Config
    <$> v .: "domain"
    <*> v .: "apiKey"
  parseJSON invalid = typeMismatch "Config" invalid

configToContext :: Config -> HailgunContext
configToContext c = HailgunContext (domain c) (apiKey c) Nothing

decodeContext :: FilePath -> IO (Either ParseException HailgunContext)
decodeContext fp = do
  parsed <- decodeFileEither fp
  return $ configToContext <$> parsed
