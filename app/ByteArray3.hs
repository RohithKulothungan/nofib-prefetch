import qualified Data.ByteString as BS
import MyBiteString(sort)
import qualified Data.ByteString.Char8 as C8

-- Function to sort characters in a string using a byte array
sortString :: String -> String
sortString input = C8.unpack $ sort $ C8.pack input

main :: IO ()
main = do
  let sortedString = sortString "trysortingthis"
  putStrLn $ "Sorted string: " ++ sortedString
