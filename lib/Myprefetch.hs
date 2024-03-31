{-# LANGUAGE MagicHash, UnboxedTuples, BangPatterns #-}

module Myprefetch (prefetchElem3, prefetchValue3) where

import GHC.IO
import GHC.Ptr
import GHC.Exts

prefetchValue3 :: a -> IO ()
prefetchValue3 x = IO (\s -> (# prefetchValue3# x s, () #))

prefetchElem3 :: [a] -> Int -> IO ()
prefetchElem3 xs i
  | i < 0 || i >= length xs = return ()
  | otherwise = let !x = xs !! i in prefetchValue3 x
