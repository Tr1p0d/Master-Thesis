{-|
  Module      : W
  Description : Marek Kidon Master Thesis 
  Copyright   : (c) Marek Kidon, 2013
  Stability   : experimental
 
  This is the module for calculating average Population individual
  in the experiments.
-}

{-# LANGUAGE RecordWildCards #-}

import Control.Monad ((<=<), void, when)
import System.IO (stderr, hPutStrLn)
import System.Exit (exitFailure)
import System.Environment (getArgs)

import Data.Vector as V (Vector, fromList)
import System.Directory (listDirectory)
import System.FilePath.Posix ((</>))

import Statistics.Quantile (continuousBy, medianUnbiased)

import SimpleStatisticsParser 
    ( Fitness(Fitness)
    , hits
    , value
    , Generation
    , RunStatistics(Run)
    , best
    , parseSimpleStatistics
    )


main :: IO ()
main = do
    args <- getArgs
    when (length args /= 1) $ error "<experiment directory>"
    (targetDir:[]) <- getArgs
    (statFiles) <- listDirectory targetDir
    bests <- V.fromList <$> mapM (onlyBest <=< parseSimpleStatistics . (targetDir </>)) statFiles
    printBox $ toHits $ computeQuantiles bests
  where
    onlyBest = either reportError (\Run{..} -> return $ value best)
    reportError e = do
      hPutStrLn stderr e
      exitFailure

computeQuantiles :: V.Vector Double -> (Double, Double, Double, Double, Double)
computeQuantiles val = thrdQ . fstQ . med . maxmin $ (0,0,0,0,0)
  where
    maxmin (_, a, b, c, _) = (continuousBy medianUnbiased 0 2 val, a, b, c, continuousBy medianUnbiased 2 2 val)
    med (a, b, _, c, d) = (a, b, continuousBy medianUnbiased 1 2 val, c, d)
    thrdQ (a, b, c, _, d) = (a, b, c, continuousBy medianUnbiased 3 4 val, d)
    fstQ (a, _, b, c, d) = (a, continuousBy medianUnbiased 1 4 val, b, c, d)

printBox :: (Show a) => (a, a, a, a, a) -> IO ()
printBox = putStrLn . reverse . tail . reverse . tail . show

toHits :: (Double, Double, Double, Double, Double) -> (Double, Double, Double, Double, Double)
toHits (a, b, c, d, e) = (toHit a, toHit b, toHit c, toHit d, toHit e)
  where
    toHit dat = (1 - dat) * 8192
