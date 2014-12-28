module Test where

import Control.Monad.State
import Control.Applicative

intState :: State Int ()
intState = do
    i <- get
    put $ i + 1

data Var a = Var a deriving (Show)
