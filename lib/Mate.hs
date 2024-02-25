module Mate (mateInN) where

import Board
import Solution
import Problem
import System.Environment( getArgs )

mateInN :: FilePath -> IO String
mateInN file = do
    putStrLn ""
    putStrLn ("File: " ++ file)
    input <- readFile file
    let (bd, (c,n)) = readProblem input
        result = showBoard bd ++
                 "\n" ++
                 show c ++ " to move and mate in " ++ show n ++ "\n" ++
                 "\n" ++
                 solve bd c n

    return result