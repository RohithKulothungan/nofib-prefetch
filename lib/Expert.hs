module Expert(process) where

import Result
import Table
import Knowledge
import Match
import Search
import System.Environment
import Control.Monad (forM_)

process :: String -> String -> String
process contents input =
    "Solving: " ++ showPhrase problem ++ "\n" ++
    display results (vars problem) replies
    where
        problem = goal (words (head (lines contents)))
        defs = definitions (tail (lines contents))
        info = enterList newTable [(q,a) | (Question q, a) <- zip results replies]
        replies = [words l /= ["no"] | l <- lines input]
        db = (defs,info)
        newsoln = Soln newTable ['X' : show n | n<-[0..]]
        results = strip [] (solve db newsoln problem)

strip qs [] = []
strip qs (Question q : rs) =
    if elem q qs then strip qs rs else
    Question q : strip (q:qs) rs
strip qs (soln:rs) = soln : strip qs rs

display [] xs as = "No (more) solutions\n"
display (Question q : rs) xs as =
    "Is it true that " ++ q ++ "?\n" ++ display rs xs (tail as)
display (Soln env vs : rs) xs as =
    "Solution: " ++ sol ++ ". More?\n" ++ etc  where
    sol = showVars env xs
    etc = if as == [] || head as == False then "" else display rs xs (tail as)

showVars env vs =
    foldr1 join (map showVar vs) where
    join x y = x ++ "; " ++ y
    showVar v = v ++ " = " ++ showPhrase (subst env (Var v))
