module Scan.Transform(minValueIndices, average) where
import qualified Data.List as L

hello = "hello from Scan.Transform"

{-
Gets a [Int] of indices of all values <= threshold value.

Used for
Reduce a row of raw image data, so that it can be smoothed out further, perhaps with average.
Pixel indice * mm/indice  == Radius 
-}
minValueIndices :: Double -> [Double] -> [Int]
minValueIndices threshold rawData  = ( L.findIndices) (<=threshold) rawData

{-
Gets the average of a list of doubles.
Used for:
Take a list of indices, such as minValueIndices, and give the average indice.
Pixel indice * mm/indice  == Radius
-}
average :: [Int] -> Double
average list = (fromIntegral $ L.sum list)  / (fromIntegral $ length list)

