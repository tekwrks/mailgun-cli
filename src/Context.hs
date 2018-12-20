module Context
  ( create
  ) where

import Control.Applicative ((<|>))
import Mail.Hailgun (HailgunContext(..))

import qualified Errors (notEnoughContext)
import qualified Config.Types as Config (Config(..))
import Flags (Flag(..), Flags)

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

fromConfig :: Maybe Config.Config -> Context
fromConfig (Just c) = Context
  (Config.domain c)
  (Config.apiKey c)
fromConfig Nothing = Context Nothing Nothing

override :: Context -> Context -> Context
override base over = Context
  (domain over <|> domain base)
  (apiKey over <|> apiKey base)

construct :: Context -> IO HailgunContext
construct Context{ domain=Just d, apiKey=Just k } =
  return $ HailgunContext d k Nothing
construct _ = Errors.notEnoughContext

create :: Flags -> Maybe Config.Config -> IO HailgunContext
create fs mc = do
  let cArgs = fromArgs fs
  let cFile = fromConfig mc
  construct $ override cFile cArgs

