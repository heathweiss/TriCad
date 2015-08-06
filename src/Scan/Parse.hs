module Scan.Parse(parseMinsToChar, parseMinsToDouble, parseMinsToRadius, average, minValueIndices) where
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.List.Split as LS
import TriCad.MathPolar( Radius(..))
import qualified Data.List as L
{-
Take image data which is given as a string, and parse it into arrays.

Tested in Tests/ParsteTest.hs
-}

{-
read for double
should it be a Maybe?

Used for: read in Doubles from [Char] that has already been split into valid doubles.
This is the data being read in from the file, after it has been split up.
-}
readDouble :: String -> Double
readDouble str = read str

{-
Gets the average of a list of doubles.
Used for:
Take a list of indices, such as minValueIndices, and give the average indice.
Pixel indice * mm/indice  == Radius
-}
average :: [Int] -> Double
average list = (fromIntegral $ L.sum list)  / (fromIntegral $ length list)


{-
Gets a [Int] of indices of all values <= threshold value.

Used for
Reduce a row of raw image data, so that it can be smoothed out further, perhaps with average.
Pixel indice * mm/indice  == Radius 
-}
minValueIndices :: Double -> [Double] -> [Int]
minValueIndices threshold rawData  = ( L.findIndices) (<=threshold) rawData

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
  



{-------- parse raw values
The format of the file must be:
"1 2;3 4$5 6;7 8"
-no spaces before or after groups of numbers
-no ; at the end of
-no $ at start or very end.
-$ separates each degree
-; separates each row
-' ' separates each image value

This is the full data from an image. So it is a 2-dim array.
It will be 1-dim data such as from gray-scale or black and white.

I need to get it into the same format of parse mins, as from there it is done.

build [[mins ints]] from file by:
 :[]
 for each $ degree (split on $)
  build [mins] by: 
   :[] for each row (split on ;)
    getAvgMinLoc of each row (split on ' ')
  build [[Radius]] by mapping Radius over the mins.
-}
