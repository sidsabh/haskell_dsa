{-# LANGUAGE InstanceSigs #-}
module Main (main) where

import System.Random
import Control.Monad.State
import Control.Monad
import Control.Monad.Loops
import Data.Foldable

newtype Vertex = V {getV :: Int} deriving Eq
instance Show Vertex where
    show (V n) = show n
newtype Weight = W {getW :: Double} deriving Eq
instance Show Weight where
    show (W n) = show n
newtype Graph = AdjMatrix [[Weight]] deriving Show

data Env = E { dist:: [Double], searching:: [Vertex] }

process :: Graph -> State Env [Int]
process (AdjMatrix graph) =
    let not_empty = (do
                    st <- get
                    let q = searching st
                    return $ (not . null) q ) :: State Env Bool
        loop = (do
            st <- get
            let q = searching st
                d = dist st
                curr_distances = [(d !! (getV x - 1), getV x) | x <- q]
                (my_dist, u) = minimumBy (\a b -> compare (fst a) (fst b)) curr_distances
                q' = filter (\i -> getV i /= u) q
                neighbors = graph !! (u-1)
                enum_neighbors = zip [1..length neighbors] neighbors
                d' = fmap update enum_neighbors where
                        update (i, W weight) =
                                let alt = my_dist + weight
                                    new = min alt (d !! (i-1))
                                in new
                e = E d' q'
            put e) :: State Env ()
        doAlgo = whileM not_empty loop
        get_answer = (do
            st <- get
            let d = dist st
            return $ round <$> d) :: State Env [Int]
    in doAlgo >> get_answer


dijkstra :: Graph -> Vertex -> [Int]
dijkstra g@(AdjMatrix graph) (V start) =
    let numNodes = length graph
        d = do 
            x <- [1..numNodes]
            if x /= start then 
                return (1/0 )
            else 
                return 0
        q = [V x | x <- [1..numNodes]]
        initEnv = E d q
    in evalState (process g) initEnv

weightMax :: Int
weightMax = 100
numNodes :: Int
numNodes = 5
startNode :: Vertex
startNode = V 1

genAdj:: RandomGen g => Int -> State g [Weight]
-- use replicateM instead of building a sequence recursively
genAdj n = replicateM n $ do
                let randWeight = do
                        p <- state uniform
                        if p then
                            fromIntegral <$> state (uniformR (1, weightMax))
                        else
                            return (1/0)
                fmap W randWeight

genGraph :: RandomGen g => Int -> g -> Graph
genGraph n =
    let buildAdj = genAdj numNodes
        buildMat = sequence [buildAdj | _ <- [1..n]]
    in AdjMatrix . evalState buildMat


main :: IO ()
main = do
    gen <- newStdGen
    let graph = genGraph numNodes gen
        dists = dijkstra graph startNode
    print graph 
    print dists