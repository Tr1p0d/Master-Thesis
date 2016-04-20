{-# LANGUAGE OverloadedStrings #-}

module SimpleStatisticsParser where

import Control.Applicative ((<|>))
import Control.Monad ((<=<), void)
import System.IO

import Data.Text.IO as T

import Data.Attoparsec.Text 


type Generation = Int
type Hits = Int

data Fitness = Fitness
    { value :: Double
    , hits :: Hits
    }
    deriving (Show)

data RunStatistics = Run 
    { progress :: [(Generation, Fitness)]
    , best :: Fitness
    }
    deriving (Show)

parseSimpleStatistics :: FilePath -> IO (Either String RunStatistics)
parseSimpleStatistics f = eitherResult . parse simpleStatisticsParser <$> T.readFile f

simpleStatisticsParser :: Parser RunStatistics
simpleStatisticsParser = do
    many' endOfLine
    progress' <- many1 fitnessStatParser
    best' <- bestFitnessParser    
    return $ Run progress' best'

fitnessStatParser :: Parser (Generation, Fitness)
fitnessStatParser = (,) <$> generationParser <*> generationFitnessParser

generationParser :: Parser Generation
generationParser = string "Generation: " >> decimal <* endOfLine

generationFitnessParser :: Parser Fitness
generationFitnessParser = do
    string "Best Individual:" <* endOfLine
    consumeLine 
    consumeLine
    fitnessParser <* consumeRest
  where
    consumeRest = consumeLine >> (endOfLine <|> consumeRest)

bestFitnessParser :: Parser Fitness
bestFitnessParser = do
    string "Best Individual of Run:" <* endOfLine
    consumeLine 
    consumeLine
    fitnessParser 

fitnessParser :: Parser Fitness
fitnessParser = do
    string "Fitness: Standardized="
    val <- double <* skipSpace
    string "Adjusted="
    void double <* skipSpace
    string "Hits="
    hits <- decimal <* endOfLine
    return $ Fitness val hits

consumeLine = takeTill isEndOfLine >> endOfLine
