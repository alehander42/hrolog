module Lib
    ( parseProlog
    ) where

import Control.Applicative((<*))
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language

parseProlog :: String -> IO ()
parseProlog source = putStrLn source
