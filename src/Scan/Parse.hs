module Scan.Parse( readDouble) where

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
