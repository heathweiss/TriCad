module Scan.Transform(minValueIndices, average, reduceRows, reduceScanRows) where
import qualified Data.List as L
import TriCad.MathPolar( Radius(..), Scan(..), SingleDegreeScan(..))
import TriCad.Types(PixelIndice, PixelValue)

{-------------------------------------- overview-----------------------------------------------
Takes a Scan.ParseAtto.RawScan datatype, and does reductions/tranformations on it, resulting in
a TriCad.MathPolar.Scan datatype, which is the datatype used for all further processing.

Reductions:
A scan image will likely have 480 rows of data, though this could vary with camera settings.
This vertical resolution is not needed for a 3D printer, so reduce the number of rows.

Transformations:
Each row from an image needs some sort of edge detection, to see the shape.

An image has to be calibrated horizontally, which is about relating edge location in the image,
to a Radius. 

Vertical calibration is left to a later stage in processing, as creating the z_axis values(vertical) is tightly coupled with
the use of TriCad.MathPolar module.
-}





{- |Returns  [PixelIndice] of all values <= threshold value.
The [PixelIndice] represents the postion of all pixels with values <= target value.
This list will still need to be reduced down to a single value, at a later stage.
-}
minValueIndices :: Double -> [PixelValue] -> [PixelIndice]
minValueIndices threshold rawData  = ( L.findIndices) (<=threshold) rawData

{-|
Gets the average of a list of PixelIndices.
Used for:
Take a list of indices, such as minValueIndices, and give the average indice.
Pixel indice * mm/indice  == Radius
-}
average :: [PixelIndice] -> Radius
average list = let temp = (fromIntegral $ L.sum list)  / (fromIntegral $ length list)
               in
                   Radius {radius=temp}

{-Take every xth element from a list.
Used for: reduce rows from the raw source data, as the usual 480 rows from an
image, is probably overkill for a 3D printer.

 (mod counter factor == 0) is saying: divide the current row index by the reduction factor, and if there is no
 remainder, include the row.

Starts of with a row index of 1, which is passed into reduceRows' to do the actual recursive work.

Must be something in Data.List such as scanl or foldl that would do this same thing.
-}
reduceRows :: Int -> [a] -> [a]
reduceRows factor x = reduceRows' factor 1 x

reduceRows' :: Int -> Int ->[a] -> [a]
reduceRows' _ _ [] = []
reduceRows' 0 _ _ = []
reduceRows' factor counter (x:xs)
        | (mod counter factor == 0) = x : reduceRows' factor (counter + 1) xs
        | otherwise            = reduceRows' factor (counter + 1) xs


{-
Reduce the of rows of data, as a scanned image will typically supply 480 rows. Way too much for a 3D scanner.
Will return a Left msg if:
- try to reduce to 0,
- by a factor larger than the available rows. Note that this only checks the 1st degree radii, as all degrees should
  have the same # of rows.

id: 1
-}
reduceScanRows :: Int -> Scan -> Either String Scan
reduceScanRows 0 _ = Left "Can't use 0 for row reduction"
reduceScanRows reduceFactor scan
  | reduceFactor > (length $ radii $ head $ degrees scan)  = Left "reduction factor > number of rows"
  | otherwise =
     let degreesReduced = [ SingleDegreeScan {degree=(degree x),  radii = (reduceRows reduceFactor $ radii x)} | x <- degrees scan]
     in  Right $  scan {degrees=degreesReduced}
        
        



