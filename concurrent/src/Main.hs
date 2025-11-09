module Main (main) where

import Control.Concurrent
import Control.Monad
import Data.Time
import Control.Concurrent.Async
import Network.HTTP
    ( getRequest, getResponseBody, simpleHTTP, Response )
import qualified Network.HTTP.Stream as Network.Stream

n :: Int
n = 100

performNetwork :: IO (Network.Stream.Result (Response String))
performNetwork =  simpleHTTP (getRequest "http://hackage.haskell.org/")

printMeasure :: (Traversable t, Show a) => IO (t (IO a)) -> IO ()
printMeasure cb = do
  start <- getCurrentTime
  writes <- cb
  k <- sequence writes
  mapM_ print k
  stop <- getCurrentTime 
  print $ diffUTCTime stop start

seqNet :: IO ()
seqNet = 
  printMeasure cb where 
    cb = do
      x <- replicateM n performNetwork  
      let writes = fmap (take 15) . getResponseBody <$> x
      return writes

perThread:: IO [Char]
perThread = do
    x <- performNetwork  
    (fmap (take 15) . getResponseBody) x

concNet :: IO ()
concNet = do
  -- make N concurrent network requests
  printMeasure cb where 
    cb = do
        spawned <- replicateM n $ async perThread
        let writes = fmap wait spawned 
        return writes 


main :: IO ()
main = do
  seqNet
  concNet