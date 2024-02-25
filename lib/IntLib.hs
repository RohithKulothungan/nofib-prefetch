module IntLib (
    readInteger,
    showInteger,
    makeNumber,
    chop,
    powerMod,
    cubeRoot,
    log2
) where

import Data.List

-- Reading and Writing
readInteger :: String -> Integer
readInteger s = read s

showInteger :: Integer -> String
showInteger i = show i

-- Interconverting between bases
makeNumber :: Integer -> [Integer] -> Integer
makeNumber b = foldl f 0
  where
    f a x = a * b + x

-- Left and right inverse of makeNumber
chop :: Integer -> Integer -> [Integer]
chop b = chop' []
  where
    chop' a n = if n == 0 then a else chop' (r : a) q
      where
        (q, r) = n `divMod` b

-- Raising a number to a power
powerMod :: Integer -> Integer -> Integer -> Integer
powerMod a 0 m = 1
powerMod a b m =
    f a' (b - 1) a'
  where
    a' = a `mod` m
    f a 0 c = c
    f a b c = g a b
      where
        g a b
            | even b = g ((a * a) `mod` m) (b `div` 2)
            | otherwise = f a (b - 1) ((a * c) `mod` m)

-- Integer Cube roots
cubeRoot :: Integer -> Integer
cubeRoot x = until satisfy improve x
  where
    satisfy y = y * y * y >= x && y' * y' * y' < x
      where
        y' = y - 1
    improve y = (2 * y * y * y + x) `ddiv` (3 * y * y)
    ddiv a b = if r < b `div` 2 then q else q + 1
      where
        (q, r) = divMod a b

-- Logarithm base 2
log2 :: Integer -> Integer
log2 = genericLength . chop 2