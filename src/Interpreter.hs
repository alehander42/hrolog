module Interpreter where
    
import Syntax

import Data.Maybe
import Data.Functor

eval :: E -> Maybe E
eval expr = Just expr