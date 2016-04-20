{-# LANGUAGE RecordWildCards #-}

import Control.Monad ((<=<), void, zipWithM)
import System.IO (stderr, hPutStrLn)
import System.Exit (exitFailure)

import qualified Data.Vector as V (Vector, empty, fromList, map)
import System.Directory (listDirectory)
import System.Environment (getArgs)

import qualified Data.Matrix as M (Matrix, fromLists, getRow, nrows, transpose, toLists)
import System.FilePath.Posix ((</>))
import Statistics.Quantile (continuousBy, medianUnbiased)

import SimpleStatisticsParser 
    ( Fitness(Fitness)
    , Generation
    , Hits
    , RunStatistics(Run)
    , hits
    , parseSimpleStatistics
    , progress
    , value
    )


main :: IO ()
main = do
    (targetDir:_) <- getArgs
    (statFiles) <- listDirectory targetDir
    stats <- M.fromLists <$> mkRunMatrix targetDir statFiles
    printTikz $ computeMedian $ M.transpose stats
  where
    mkRunMatrix target = mapM (vectorProgress <=< handleError <=< parseSimpleStatistics . (target </>))
    computeMedian = rowWise (continuousBy medianUnbiased 1 2) 
    vectorProgress = return . map (value . snd) . progress
    handleError = either reportError return
    reportError e = do
      hPutStrLn stderr e
      exitFailure

rowWise :: (V.Vector a -> b) -> M.Matrix a -> [b]
rowWise op mat = map (\rowN -> op (M.getRow rowN mat)) [1 .. M.nrows mat]


printTikz :: [Double] -> IO ()
printTikz = void . zipWithM (\g f -> putStrLn $ show (g, (1-f) * 8192)) [0..]
  where
    showGenerationF (g, Fitness{..}) = show $ (g, hits) 
