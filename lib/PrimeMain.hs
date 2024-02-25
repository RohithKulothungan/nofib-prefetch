
module PrimeMain(process) where

import IntLib
import MyRandom
import Prime

-- The process function takes a list of input lines and produces a list
-- of output lines.

process :: [String] -> [String]
process = doInput initState

-- To do this, we consider each input line in turn in doInput; this
-- passes along the state.

doInput :: State -> [String] -> [String]
doInput state []     = []
doInput state (l:ls) = doLine l (\state -> doInput state ls) state

-- The doLine function processes an individual line.

doLine :: String -> (State -> [String]) -> State -> [String]
doLine cs cont rs =
    if t then "Probably prime" : rest else "Composite" : rest
  where
    n        = readInteger cs
    (t, rs') = multiTest 100 rs n
    rest     = cont rs'

-- For the particular problem we have in mind, we make the following
-- definitions.

type State = [Int]
initState  = randomInts 111 47
