module Variables.Types where

import qualified Data.Text as T

type Variable = (String, String)
type Variables = [Variable]

find :: Variables -> String -> Maybe T.Text
find [] _ = Nothing
find ((k,v):vs) key =
  if k == key
     then Just $ T.pack v
     else find vs key

