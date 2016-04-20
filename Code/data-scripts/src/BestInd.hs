{-# LANGUAGE RecordWildCards #-}

import Control.Monad ((<=<), void)
import System.IO (stderr, hPutStrLn)
import System.Exit (exitFailure)
import System.Environment (getArgs)

import qualified Data.Vector as V (Vector, (!), fromList, maxIndex)
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
    (targetDir:outpurFile:[]) <- getArgs
    (statFiles) <- listDirectory targetDir
    bests <- V.fromList <$> mapM (onlyBest <=< parseSimpleStatistics . (targetDir </>)) statFiles
    print (statFiles !! V.maxIndex bests, bests V.! V.maxIndex bests)
  where
    onlyBest = either reportError (\Run{..} -> return $ hits best)
    reportError e = do
      hPutStrLn stderr e
      exitFailure
