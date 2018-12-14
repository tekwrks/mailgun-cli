module Context.Types
  ( Context(..)
  ) where

data Context = Context
  { domain :: Maybe String
  , apiKey :: Maybe String
  } deriving (Eq, Show)

