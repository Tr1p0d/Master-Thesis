-- |
-- Module      : Main
-- Description : The ip hash function GP evolution.
-- Copyright   : (c) Marek Kidon, 2016
-- License     : BSD
-- Maintainer  : xkidon00@stud.fit.vutbr.cz
-- Stability   : experimental
-- Portability :
--
-- This module contains specification for GP evolution
-- for developing IP hash functions.
--
module Main where

import Control.Monad ((>=>))
import Data.Bits ((.&.), (.|.), xor, shift)

import Control.Monad.Primitive (PrimMonad)
import qualified Data.Vector as V (mapM)
import qualified Data.Set as Set (empty, insert, member, size)

import Control.Lens ((^.))
import System.Random.MWC (withSystemRandom)

import AI.GP
    ( Evolution
    , GPEvolutionParams(GPEvolutionParams)
    , evalEvolution
    , evolve
    , runEvolution
    )
import AI.GP.Type.Evolution (individual)
import AI.GP.Type.Fitnesse (individualScore)
import AI.GP.Selection (tournamentSelection)
import AI.GP.Init (fullInit, growInit)
import AI.GP.Crossover (subtreeCrossoverUniform)
import AI.GP.Mutation (pointMutationPreferLeafs, subtreeMutationUniform)
import AI.GP.Type.Fitnesse (discardFitness)
import AI.GP.Type.GProgram
import Utils (readAdresses)


data Operators
    = Times
    | Plus
    | Minus
    | AND
    | OR
    | XOR
--    | SHL
--    | SHR
    deriving (Show)

allOperators = [Times, Plus, Minus, XOR{-, ShiftL, ShiftR-}]

data Terminals
    = Octet1
    | Octet2
    | Octet3
    | Octet4
    deriving (Show)

allTerminals = [Octet1, Octet2, Octet3, Octet4]

eval :: [Int] -> GProgram Operators Terminals -> Int
eval octets (Node _ Plus l r) = eval octets l + eval octets r
eval octets (Node _ Times l r) = eval octets l * eval octets r
eval octets (Node _ Minus l r) = eval octets l - eval octets r
eval octets (Node _ AND l r) = eval octets l .&. eval octets r
eval octets (Node _ OR l r) = eval octets l .|. eval octets r
eval octets (Node _ XOR l r) = eval octets l `xor` eval octets r
--eval octets (Node _ SHL l r) = eval octets l * eval octets r
--eval octets (Node _ SHR l r) = eval octets l - eval octets r
eval (o1:_) (Leaf _ Octet1) = o1
eval (_:o2:_) (Leaf _ Octet2) = o2
eval (_:_:o3:_) (Leaf _ Octet3) = o3
eval (_:_:_:o4:[]) (Leaf _ Octet4) = o4

fitness :: GProgram Operators Terminals -> IO Double
fitness prog = do
    addresses <- readAdresses "data/cesnet_addrs_8192a"
    let total = length addresses
        hashed = hash prog addresses Set.empty
    return $ fromIntegral $ total - hashed
  where
    hash prog [] s = Set.size s
    hash prog (a:as) s =
        if Set.member (eval a prog) s
        then Set.size s
        else hash prog as (Set.insert (eval a prog) s)

main :: IO ()
main = withSystemRandom $ evalEvolution ipHash
    >=> print . head

ipHashParams :: {-(PrimMonad m) =>-} GPEvolutionParams IO Operators Terminals
ipHashParams = GPEvolutionParams
    100
    (growInit allOperators allTerminals 5)
    (tournamentSelection 3 20) -- ^ use tournament selection
    fitness
    (< 1.0)
    99 -- ^ one ellite individuals.
    subtreeCrossoverUniform
    0.1
    (subtreeMutationUniform allOperators allTerminals)
    (\_ p -> V.mapM (return . discardFitness) p)
    100

ipHash = evolve ipHashParams
