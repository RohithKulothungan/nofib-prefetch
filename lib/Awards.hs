{-
Date: Thu, 22 Jun 95 15:39:44 BST
From: kh@dcs.gla.ac.uk
Message-Id: <9506221439.AA07592@tuvula.dcs.gla.ac.uk>
To: partain@dcs.gla.ac.uk
Subject: nofib submission
Cc: trinder@dcs.gla.ac.uk

A little program written as a demonstrator to show someone how to solve
a real problem.  I make no claims of elegance or efficiency.  The data
is fake (Phil may want to improve this!).

Kevin
-}

-- In a public award scheme, each entrant can receive awards
-- based on their scores in a number of events.  To achieve an
-- award, the competitor must have competed in three events
-- and achieved a total score greater than the fixed threshold
-- for the award.

-- No score can be counted towards more than one award,
-- but there is no limit on the total number of awards that can be won.

-- The thresholds for the various awards are:
--	Gold	70 points
--	Silver	60 points
--	Bronze	50 points
module Awards (findallawards) where

import QSort
import qualified Data.List as DL 
import MyLib((\\))
import System.Environment
import Control.Monad

-- Generate all possible permutations of length m from a list of scores
perms m [] = []
perms 1 l  = map (: []) l
perms m (n:ns) = map ((:) n) (perms (m-1) ns) ++ perms m ns

-- Find the (sorted) list of possible awards for a list of scores
awards scores =
	award ("Gold",70) ++ award ("Silver",60) ++ award ("Bronze",50)
	where sumscores = map (\ p -> (sum p, p)) (perms 3 scores)
	      atleast threshold = filter (\(sum,p) -> sum >= threshold) sumscores
	      award (name,threshold) = map (\ ps -> (name,ps)) (sort (atleast threshold))

-- Find all possible awards for a list of scores, counting each score once only
findawards scores | null theawards = []
  	          | otherwise = firstaward : findawards (scores \\ perm)
	where firstaward@(award,(sum,perm)) = head theawards
	      theawards = awards scores

-- Find the awards for all competitors, each competitor is a pair of
-- (Name, list of scores)
findallawards competitors =
	map (\ (name,scores) -> (name,findawards scores)) competitors


-- main :: IO ()
-- main = do
--   (n:_) <- getArgs
--   defaultMain [ bgroup "findallawards" [benchmarkFunction i | i <- [1..read n]] ]

-- main = do
--   (n:_) <- getArgs
--   forM_ [1..read n] $ \i -> do
--     print (findallawards (competitors (i `mod` 100)))