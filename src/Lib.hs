module Lib
    ( parseProlog
    ) where

import Text.Parsec.String (Parser)
import Data.Char
import Text.Parsec.Combinator (many1)

parseProlog :: String -> IO ()
parseProlog source = putStrLn source
