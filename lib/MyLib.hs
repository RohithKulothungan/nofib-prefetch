module MyLib
    ( someFunction
    , lIndex
    , (\\)
    ) where
import Data.List (delete)

infix 5 \\ 

someFunction :: String -> String
someFunction input = "Hello, " ++ input

lIndex :: [a] -> Int -> a
xs `lIndex` n | n < 0 = errorWithoutStackTrace "MyLib.!!: negative index"
[] `lIndex` _ = errorWithoutStackTrace "MyLib.!!: index too large"
(x:_) `lIndex` 0 = x

(_:xs) `lIndex` n = xs `lIndex` (n-1)

(\\)                    :: (Eq a) => [a] -> [a] -> [a]
(\\)                    =  foldl (flip delete)
