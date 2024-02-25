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
--   bgroup "Ind" [ bench "1000"  $ nf generateList 1000
--                , bench "10k"  $ nf generateList 10000
--                , bench "100k"  $ nf generateList 100000
--                ]
--   ]
module Main where

import Criterion
import Criterion.Types
import Criterion.Main
import System.Environment
import Awards
import Banner
import Calendar
import Prog
import Circsim
import Constraints
import Cryptarithm2
import Dom
import Data.Bifunctor
import Control.Monad
import Data.Traversable
import Expert
import Mate
import Sort
import Numbers
import Vectors
import MyIO
import Hide
import NofibUtils
import InferMain
import PrologMain
import PrimeMain

competitors i =
  [ ("Simon",[35,27,40,i,34,21])
  , ("Hans",[23,19,45,i,17,10,5,8,14])
  , ("Phil",[1,18,i,20,21,19,34,8,16,21])
  , ("Kevin",[9,23,17,54,i,41,9,18,14])
  ]

benchmarkFunction :: Int -> Benchmark
benchmarkFunction i = bench (show i) $ nf Awards.findallawards (competitors (i `mod` 100))

benchmarkForCichelli :: Int -> Benchmark
benchmarkForCichelli n = bench ("prog " ++ show n) $ nf Prog.prog n

runDominatorBenchmark :: FilePath -> IO ()
runDominatorBenchmark inputFile = do
  sgraphs <- map (second Dom.fromAdj . read) . lines <$> readFile inputFile :: IO [Rooted]
  let s = sum [doGraph g | g <- sgraphs] :: Int
  print s

doGraph :: Rooted -> Int
doGraph g = length . Dom.idom $ g

main :: IO ()
main = do
    defaultMainWith(defaultConfig {reportFile = Just ("findallawards-benchmark.html")})[ bgroup "findallawards" [benchmarkFunction i | i <- [1..3]] ]
    let stuff = "Hello world \n this is a test string \n 123243255345 \n file writing"
    defaultMainWith(defaultConfig {reportFile = Just ("Banner-benchmark.html")}) [bench "Banner Say" $ nf (map (Banner.say) . lines) stuff]
    defaultMainWith(defaultConfig {reportFile = Just ("Calendar-benchmark.html")}) [ bgroup "calendar"
                                                                                        [ bench "calendar" $ nf (length . Calendar.calendar) 2022
                                                                                        , bench "cal" $ nf (length . Calendar.cal) 2022
                                                                                        ]
                                                                                    ]
    defaultMainWith(defaultConfig {reportFile = Just ("Cichelli-benchmark.html")}) [bgroup "Cichelli" [benchmarkForCichelli (3)]]
    defaultMainWith(defaultConfig {reportFile = Just ("Circuit-Simulator-benchmark.html")}) [bgroup "Circuit Simulator" [Circsim.benchmarkForCircuitSimulator 1000]]
    defaultMainWith(defaultConfig {reportFile = Just ("CSP-benchmark.html")}) [ bgroup "CSP Benchmarks"
                                                                                  [ bench "Backtracking" $ nfIO (Constraints.runSolver Constraints.bt)
                                                                                  , bench "Backmarking" $ nfIO (Constraints.runSolver Constraints.bm)
                                                                                  , bench "Backjumping" $ nfIO (Constraints.runSolver Constraints.bjbt)
                                                                                  , bench "Forward Checking" $ nfIO (Constraints.runSolver Constraints.fc)
                                                                                  ]
                                                                                ]
    defaultMainWith(defaultConfig {reportFile = Just ("Cryptarithm-benchmark.html")})[ bench "Cryptarithm Solver" $ nfIO Cryptarithm2.runCryptarithmSolver]
    defaultMainWith(defaultConfig {reportFile = Just ("Compute-Dominators-benchmark.html")})[ bench "Compute Dominators" $ nfIO (runDominatorBenchmark "lib/ghc-examples.in")]
    animals <- readFile "lib/runtime_files_expert/animals"
    contents <- readFile "lib/runtime_files_expert/expert"
    defaultMainWith(defaultConfig {reportFile = Just ("Mate-benchmark.html")})
                                                                              [ bench "process Mate" $ nfIO $
                                                                                  forM_ [1..999] $ \i ->
                                                                                      print
                                                                                      . length
                                                                                      . Expert.process animals
                                                                                      . take (i + 9999)
                                                                                      $ contents
                                                                              ]
    let files = ["lib/runtime_files_mate/heathcote3.prob"]  -- Add your file name here or modify the list as needed
    defaultMainWith(defaultConfig {reportFile = Just ("Mate-benchmark.html")}) (map (\file -> bench file $ whnfIO (Mate.mateInN file)) files)
    cts <- readFile "lib/primetest.stdin"
    let inputLines = lines cts
    let benchmark =  defaultMainWith(defaultConfig {reportFile = Just ("PrimeTest-benchmark.html")})[ bench "process primetest" $ whnf PrimeMain.process inputLines] 
    _ <- benchmark
    mapM_ putStr (PrimeMain.process inputLines)
    -- let inputHidden = "2,10,3\nquit\n"
    -- Problem to match the criterion benchmark on the function process.
    -- replicateM_ 20 $ do
    --     ls <- NofibUtils.salt inputHidden
    --     (getFilename $
    --         MyIO.process (\viewdir -> Hide.hiddenline viewdir . map read . lines)) (lines ls)
    input_infer <- readFile "lib/infer.stdin"
    defaultMainWith(defaultConfig {reportFile = Just ("Inference-benchmark.html")})[ bench "Benchmarking inference" $ nfIO (replicateM_ 200 $ InferMain.doInference input_infer)]
    defaultMainWith(defaultConfig {reportFile = Just ("Interpreter-benchmark.html")})[ bench "interpreter" $ nfIO PrologMain.runInterpreter]
    defaultMainWith(defaultConfig {reportFile = Just ("Sort-benchmark.html")}) [bgroup "sortBenchmarks" [
                    bench "heapSort" $ nf (Sort.benchmarkSort Sort.heapSort) input
                  , bench "insertSort" $ nf (Sort.benchmarkSort Sort.insertSort) input
                  , bench "mergeSort" $ nf (Sort.benchmarkSort Sort.mergeSort) input
                  , bench "quickSort" $ nf (Sort.benchmarkSort Sort.quickSort) input
                  , bench "quickSort2" $ nf (Sort.benchmarkSort Sort.quickSort2) input
                  , bench "quickerSort" $ nf (Sort.benchmarkSort Sort.quickerSort) input
                  , bench "treeSort" $ nf (Sort.benchmarkSort Sort.treeSort) input
                  , bench "treeSort2" $ nf (Sort.benchmarkSort Sort.treeSort2) input
                  ]
                ]
                where
                  input = "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Quisque tincidunt mauris ac nulla ullamcorper, et varius ligula bibendum. Integer ut felis eget tellus aliquet dignissim. Sed varius odio vel lacus tristique, vel semper libero rhoncus. Vestibulum sed orci id justo pulvinar sagittis. Suspendisse potenti. Vestibulum sit amet justo ac libero fringilla suscipit. Sed vel tincidunt risus. Vestibulum id venenatis mauris. Suspendisse potenti. Fusce et vehicula lacus. Sed ultrices orci vitae vestibulum eleifend. Donec non justo vel purus sagittis euismod in vel metus."