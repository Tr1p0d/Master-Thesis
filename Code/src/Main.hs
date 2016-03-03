module Main where

import Control.Monad (replicateM)
import Control.Monad.Identity (Identity(Identity))

import AI.GP.Init

main :: IO ()
main = let a=a in a

data Op = Plus | Minus deriving (Show)
data Term = Const Int deriving (Show)

plusTree = full (Identity Plus) (Identity (Const 1)) 2
minusTree = full (Identity Minus) (Identity (Const 1)) 2

population = replicateM 3 plusTree
