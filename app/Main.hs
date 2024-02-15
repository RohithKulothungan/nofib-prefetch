-- module Main where

-- import qualified MyLib

-- main :: IO ()
-- main = do
--   let xs = replicate 1000 1
--   let ys = map ( \i -> xs `MyLib.lIndex` i) [0..999]
--   print ys


-- import qualified MyLib
-- import Criterion.Main

-- generateList :: Int -> [Int]
-- generateList limit = map (\i -> replicate limit 1 `MyLib.lIndex` i) [0..limit-1]

-- -- The function we're benchmarking.
-- fib m | m < 0     = error "negative!"
--       | otherwise = go m
--   where
--     go 0 = 0
--     go 1 = 1
--     go n = go (n-1) + go (n-2)

-- -- Our benchmark harness.
-- main = defaultMain [
--   bgroup "Ind" [ bench "1000"  $ whnf generateList 1000
--                , bench "10k"  $ whnf generateList 10000
--                , bench "100k"  $ whnf generateList 100000
--                ]
--   ]

import Criterion.Main
import System.Environment (getArgs)
import Awards

benchmarkFunction :: Int -> Benchmark
benchmarkFunction i = bench (show i) $ nf (Awards.findallawards(competitors(i `mod` 100)) i

main :: IO ()
main = do
    (n:_) <- getArgs
    defaultMain [ bgroup "findallawards" [benchmarkFunction i | i <- [1..read n]] ]
