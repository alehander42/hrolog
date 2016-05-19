module Syntax where

import Data.String

-- data Nam = Name String deriving (Eq, Show)
-- data Name = Name String deriving (Eq, Show)
data E = Name String | Fact E [E] | Unsuccesfull
  deriving (Eq, Show)

toName :: String -> E
toName s = Name s

