import qualified Data.ByteString as BS
import MyByteString(sort, pack, unpack)
import qualified Data.ByteString.Char8 as C8
import Criterion
import Criterion.Types
import Criterion.Main
import System.Environment

-- Function to sort characters in a string using a byte array
sortString :: String -> String
sortString input = unpack $ sort $ pack input

main :: IO ()
main = do
  defaultMainWith(defaultConfig {reportFile = Just ("Sorting-ByteArray-benchmark.html")}) [ bgroup "ByteArraySort"
                                                                                                [bench "Sorting ByteArray" $ nf sortString "trysortingthis"
                                                                                                , bench "Sorting ByteArray" $ nf sortString "thisisthelongestofallinputs"
                                                                                                , bench "Sorting ByteArray" $ nf sortString "willibeabletofinishthedissertation"
                                                                                                ]
                                                                                            ] 
