module Scan.Parse.Mins(parseMinsToChar, parseMinsToDouble, parseMinsToRadius ) where
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.List.Split as LS
import Scan.Parse(readDouble)
import TriCad.MathPolar( Radius(..))

hello = "hello"

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
rather than processing it in C++.
-}

parseMinsToChar :: BL.ByteString -> [[[Char]]]
parseMinsToChar bs =
  let splitColon =  LS.splitOn ";"   $ BL.unpack bs
      splitSpace =  map (LS.splitOn " ") splitColon
  in  splitSpace
  
parseMinsToDouble ::  BL.ByteString -> [[Double]]
parseMinsToDouble bs =  [ map (readDouble) x |  x <- parseMinsToChar bs]

parseMinsToRadius bs =  [ map (Radius . readDouble) x |  x <- parseMinsToChar bs]

