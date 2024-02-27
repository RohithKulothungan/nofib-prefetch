{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE Trustworthy #-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module MyByteString where

import Prelude hiding           (reverse,head,tail,last,init,Foldable(..)
                                ,map,lines,unlines
                                ,concat,any,take,drop,splitAt,takeWhile
                                ,dropWhile,span,break,filter
                                ,all,concatMap
                                ,scanl,scanl1,scanr,scanr1
                                ,readFile,writeFile,appendFile,replicate
                                ,getContents,getLine,putStr,putStrLn,interact
                                ,zip,zipWith,unzip,notElem
                                )



import Data.Word                (Word8)
import Control.Monad            (when)
import Foreign.Marshal.Array    (allocaArray)
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable         (Storable(..))
import MyInternalTypes
import GHC.Prim

unpack :: ByteString -> [Char]
unpack = unpackChars

pack :: String -> ByteString
pack = packChars

sort :: ByteString -> ByteString
sort (BS input l)
  -- qsort outperforms counting sort for small arrays
  | l <= 20 = unsafeCreateFp l $ \destFP -> do
    memcpyFp destFP input l
    unsafeWithForeignPtr destFP $ \dest -> c_sort dest (fromIntegral l)
  | otherwise = unsafeCreateFp l $ \p -> allocaArray 256 $ \arr -> do

    fillBytes (castPtr arr) 0 (256 * sizeOf (undefined :: Int))
    unsafeWithForeignPtr input (\x -> countOccurrences arr x l)

    let go 256 !_   = return ()
        go i   !ptr = do n <- peekElemOff arr i
                         when (n /= 0) $
                           fillBytes ptr (fromIntegral @Int @Word8 i) n
                         go (i + 1) (ptr `plusPtr` fromIntegral n)
    unsafeWithForeignPtr p (go 0)
  where
    -- Count the number of occurrences of each byte.
    countOccurrences :: Ptr Int -> Ptr Word8 -> Int -> IO ()
    countOccurrences !counts !str !len = go 0
     where
        go !i | i == len    = return ()
              | otherwise = do k <- fromIntegral `fmap` peekElemOff str i
                               x <- peekElemOff counts k
                               pokeElemOff counts k (x + 1)
                               -- Inserting prefetch hint
                               go (i + 1)