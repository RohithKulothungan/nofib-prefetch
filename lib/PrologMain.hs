module PrologMain(runInterpreter) where

import PrologData
import ParseProlog
import Interact
import Subst
import Engine
import Version
import MyLib(nub)
import qualified Data.List as DL --1.3
import Control.Monad
import System.Environment
import System.IO.Error (catchIOError)
import NofibUtils

import Criterion.Main

--- Command structure and parsing:

data Command = Fact Clause | Query [Term] | Show | Error | Quit | NoChange

command :: Parser Command
command  = just (sptok "bye" `orelse` sptok "quit") `doo` (\quit->Quit)
               `orelse`
           just (okay NoChange)
               `orelse`
           just (sptok "??") `doo` (\show->Show)
               `orelse`
           just clause `doo` Fact
               `orelse`
           just (sptok "?-" `seQ` termlist) `doo` (\(q,ts)->Query ts)
               `orelse`
           okay Error

--- Main program read-solve-print loop:

signOn           :: String
signOn            = "Mini Prolog Version 1.5 (" ++ version ++ ")\n\n"

runInterpreter :: IO ()
runInterpreter = putStr signOn >>
                    putStr ("Reading " ++ stdlib) >>
                    catchIOError (readFile stdlib)
                      (\fail -> putStr "...not found\n" >> return "")
                      >>= \ is ->
                    if null is then
                       interpreter []
                    else
                      let parse   = map clause (lines is)
                          clauses = [ r | ((r,""):_) <- parse ]
                          reading = ['.'| c <- clauses] ++ "done\n"
                      in
                      putStr reading >>
                      interpreter clauses

stdlib           :: String
stdlib            = "lib/runtime_files_prolog/stdlib"

interpreter      :: [Clause] -> IO ()
interpreter lib   = do
  let startDb = foldl addClause emptyDb lib
  is <- getContents
  replicateM_ 200 $ do
    is' <- salt is
    print (hash (loop startDb is'))

loop             :: Database -> String -> String
loop db           = readln "> " (exec db . fst . head . command)

exec             :: Database -> Command -> String -> String
exec db (Fact r)  = skip                              (loop (addClause db r))
exec db (Query q) = demonstrate db q
exec db Show      = writeln (show db)                 (loop db)
exec db Error     = writeln "I don't understand\n"    (loop db)
exec db Quit      = writeln "Thank you and goodbye\n" end
exec db NoChange  = skip                              (loop db)

--- Handle printing of solutions etc...

solution      :: [Id] -> Subst -> [String]
solution vs s  = [ show (Var i) ++ " = " ++ show v
                                | (i,v) <- [ (i,s i) | i<-vs ], v /= Var i ]

demonstrate     :: Database -> [Term] -> Interactive
demonstrate db q = printOut (map (solution vs) (prove db q))
 where vs               = (nub . concat . map varsIn) q
       printOut []      = writeln "no.\n"     (loop db)
       printOut ([]:bs) = writeln "yes.\n"    (loop db)
       printOut (b:bs)  = writeln (doLines b) (nextReqd bs)
       doLines          = foldr1 (\xs ys -> xs ++ "\n" ++ ys)
       nextReqd bs      = writeln " "
                            (readch (\c->if c==';'
                                           then writeln ";\n" (printOut bs)
                                           else writeln "\n"  (loop db)) "")
