module Scan.Parse.Mins(parseToChar, parseToDouble, parseToRadius ) where
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.List.Split as LS
import Scan.Parse(readDouble)
import TriCad.MathPolar( Radius(..))



{---------- parse min values
The format of the file must be:
"1 2;3 4;5 6"
-no  spaces before or after a group of numbers

-no trailing ;
-' ' separates each min value
-; separates each degree

Because the image had each row reduced to a postion of min value,
each ';' sep'd set, represents a scan at a degree.

It will be 1-dim data such as from gray-scale or black and white.

It is possible that all the mins will be removed, as it may be better to have full data in haskell,
rather than processing it in opencv.

Testing done in Tests/ParseMinsTest.hs
-}

parseToChar :: BL.ByteString -> [[[Char]]]
parseToChar bs =
  let splitColon =  LS.splitOn ";"   $ BL.unpack bs
      splitSpace =  map (LS.splitOn " ") splitColon
  in  splitSpace
  
parseToDouble ::  BL.ByteString -> [[Double]]
parseToDouble bs =  [ map (readDouble) x |  x <- parseToChar bs]

parseToRadius bs =  [ map (Radius . readDouble) x |  x <- parseToChar bs]

