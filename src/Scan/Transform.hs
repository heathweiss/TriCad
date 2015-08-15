module Scan.Transform(minValueIndices, average, reduceRows) where
import qualified Data.List as L

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





{-
Gets a [Int] of indices of all values <= threshold value.

Used for
Reduce a row of raw image data, to a [Double] that represents the pixel location for all values <= a threshold value.
This can be smoothed out further, perhaps with average. 

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

{-Take every xth element from a list.
Used for: reduce rows from the raw source data, as the usual 480 rows from an
image, is probably overkill.
-}
reduceRows :: Int -> [a] -> [a]
reduceRows factor x = reduceRows' factor 1 x

reduceRows' :: Int -> Int ->[a] -> [a]
reduceRows' _ _ [] = []
reduceRows' 0 _ _ = []
reduceRows' factor counter (x:xs)
        | (mod counter factor == 0) = x : reduceRows' factor (counter + 1) xs
        | otherwise            = reduceRows' factor (counter + 1) xs
