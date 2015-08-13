{-# LANGUAGE ParallelListComp #-}
module Scan.Parse( readDouble, parseToScan) where
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.List.Split as LS
import TriCad.MathPolar( Radius(..),Degree(..), Scan(..))
import Scan.Transform(minValueIndices, average)

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



parseToScan :: ([Double] -> Double) -> [Char] -> Scan
parseToScan f str = 
                      let splitToSpaces = ((map (LS.splitOn ";") (LS.splitOn "$" str)))
                          splitToChars = [map (LS.splitOn " ") x | x <- splitToSpaces]
                          degreeValsMulti =  [[readDouble y | y:ys <-  x] | x <- splitToChars ]
                          degreeVals = map head degreeValsMulti
                          readToDoublesNoDegreeVals = [[map ( readDouble) ys | y:ys <-  x] | x <- splitToChars ]
                          readToRadius =  [[((Radius) . f ) y  | y  <-  x] | x <- readToDoublesNoDegreeVals ]
                          degrees = zipWith (Degree) degreeVals readToRadius
                          scan = Scan {name= "myScan", degrees= degrees}
                        in
                        scan


testFromString = (parseToScan (average . minValueIndices 5 ) "0 1 3 6;0 3 4 5 7$90 2 4 6; 90 3 4 8")

{-
The use of LS.splitOn is what I have to do to make it compile, and have correct output. WTF?
If I omit the parseToScan type signature, I can get rid of LS.splitOn "%", but then the output is incorrect
-}
testFromContrivedByteString = do contents <- LS.splitOn "%" $  BL.unpack $ BL.pack "0 1 3 6;0 3 4 5 7$90 2 4 6; 90 3 4 8" 
                                 let t = (parseToScan (average . minValueIndices 5 ) contents)
                                 show t

testFromFile = do contents <-   BL.readFile "src/Data/sample.raww"
                  let temp =  BL.unpack contents
                  print $ show $  (parseToScan (average . minValueIndices 5 ) temp)





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
