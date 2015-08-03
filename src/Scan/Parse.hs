module Scan.Parse(parseToChar, parseToDouble, parseToRadius) where
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.List.Split as LS
import TriCad.MathPolar( Radius(..))

parseToChar :: BL.ByteString -> [[[Char]]]
parseToChar bs =
  let splitColon =  LS.splitOn ";"   $ BL.unpack bs
      splitSpace =  map (LS.splitOn " ") splitColon
  in  splitSpace
  
parseToDouble ::  BL.ByteString -> [[Double]]
parseToDouble bs =  [ map (readDouble) x |  x <- parseToChar bs]

parseToRadius bs =  [ map (Radius . readDouble) x |  x <- parseToChar bs]
  

readDouble :: String -> Double
readDouble str = read str

