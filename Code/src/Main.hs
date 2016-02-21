{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Prelude

import AI.GP.Class.Population (sample)

import Control.Applicative ((<$>))
import Control.Monad (replicateM, return)
import Control.Monad.Random (Rand, StdGen, getRandom,  getRandomR, evalRandIO)
import Data.Either (Either(Left,Right))
import Data.Function (($), const)
import Data.Int (Int)
import Data.Proxy (Proxy)
import Data.Traversable (sequence)
import qualified Data.Vector as V (Vector, empty, fromList, length, replicateM)
import System.IO (IO)
import Text.Show (Show)

import AI.GP
import AI.GP.Class.Population

main ::  IO ()
main = let a=a in a

--data E
--    = Plus E E
--    | Const Double

