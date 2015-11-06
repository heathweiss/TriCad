{-|

Module          : Scan.ParseAtto
Description     : Transform PixelValues to Radius


Takes a Scan.ParseAtto.PixelValueScan datatype, and does reductions/tranformations on it, resulting in
a TriCad.MathPolar.Scan datatype, which is the datatype used for all further processing.

Reductions:
A scan image will likely have 480 rows of data, though this could vary with camera settings.
This vertical resolution is not needed for a 3D printer, so reduce the number of rows.

Transformations:
Each row starts as pixel values, which need some sort of edge detection, to see the shape.

An image has to be calibrated horizontally, which is about relating edge location in the image,
to a Radius. 

Vertical calibration is left to a later stage in processing, as creating the z_axis values(vertical) is tightly coupled with
the use of TriCad.MathPolar module.
-}


module Scan.Transform(pixelIndicesOfPixelValuesLTE, pixelIndicesAverageToRadius, reduceRows, reduceScanRows, reduceScan ,
                     RowReductionFactor(..), multiDegreePixelValuesToMultiDegreeRadii ) where
import qualified Data.List as L
import CornerPoints.Radius( Radius(..))
import qualified  Scan.Parse as PA  (MultiDegreePixelValues(..), PixelValue(..),SingleDegreePixelValues(..))
import CornerPoints.VerticalFaces(SingleDegreeRadii(..), MultiDegreeRadii(..))


-- |The indice(position) of a pixel in a the image taken for a scan.
--The indice is the radius, though it will still need to be converted to a distance.
type PixelIndice = Int



-- |Name of the scan.
type Name = String


--type ThreshholdValue = Double
{- |Returns  [PixelIndice] of all values <= PixelValue threshold value.
The [PixelIndice] represents the postion of all pixels with values <= target value.
This list will still need to be reduced down to a single value, at a later stage.
-}
pixelIndicesOfPixelValuesLTE :: PA.PixelValue -> [PA.PixelValue] -> [PixelIndice]
pixelIndicesOfPixelValuesLTE thresholdValue rawData  = ( L.findIndices) (<=thresholdValue) rawData

{-|
Gets the average of a list of PixelIndices.
Used for:
Take a list of indices, such as minValueIndices, and give the average indice.
Pixel indice * mm/indice  == Radius
-}

pixelIndicesAverageToRadius :: [PixelIndice] -> Radius
pixelIndicesAverageToRadius list = let temp = (fromIntegral $ L.sum list)  / (fromIntegral $ length list)
               in
                   Radius {radius=temp}
{-
should I watch for a zero value.
And if there is one, what to use for a default



pixelIndicesAverageToRadius :: [PixelIndice] -> Radius
pixelIndicesAverageToRadius list =
               let temp =
                     if ((length list)) == 0
                        then 0
                     else (fromIntegral $ L.sum list)  / (fromIntegral $ length list)
               in
                   Radius {radius=temp}
-}



{-
Reduce the of rows of data, as a scanned image will typically supply 480 rows. Way too much for a 3D scanner.
Will return a Left msg if:
- try to reduce to 0,
- by a factor larger than the available rows. Note that this only checks the 1st degree radii, as all degrees should
  have the same # of rows.

id: 1
-}
reduceScanRows :: Int -> MultiDegreeRadii -> Either String MultiDegreeRadii
reduceScanRows 0 _ = Left "Can't use 0 for row reduction"
reduceScanRows reduceFactor scan
  | reduceFactor > (length $ radii $ head $ degrees scan)  = Left "reduction factor > number of rows"
  | otherwise =
     let degreesReduced = [ SingleDegreeRadii {degree=(degree x),  radii = (reduceRows reduceFactor $ radii x)} | x <- degrees scan]
     in  Right $  scan {degrees=degreesReduced}

reduceScan :: Int -> MultiDegreeRadii -> MultiDegreeRadii
reduceScan reduceFactor scan =
  
     let degreesReduced = [ SingleDegreeRadii {degree=(degree x),  radii = (reduceRows reduceFactor $ radii x)} | x <- degrees scan]
     in  scan {degrees=degreesReduced}

{- Take every xth element from a list.
Used for: reduce rows from the raw source data, as the usual 480 rows from an
image, is probably overkill for a 3D printer.

 (mod counter factor == 0) is saying: divide the current row index by the reduction factor, and if there is no
 remainder, include the row.

Starts of with a row index of 1, which is passed into reduceRows' to do the actual recursive work.

Must be something in Data.List such as scanl or foldl that would do this same thing.
-}
reduceRows :: RowReductionFactor -> [a] -> [a]
reduceRows factor x = reduceRows' factor 1 x

reduceRows' :: RowReductionFactor -> Int ->[a] -> [a]
reduceRows' _ _ [] = []
reduceRows' 0 _ _ = []
reduceRows' factor counter (x:xs)
        | (mod counter factor == 0) = x : reduceRows' factor (counter + 1) xs
        | otherwise            = reduceRows' factor (counter + 1) xs

-- |Factor by which to reduce rows.
type RowReductionFactor = Int
        
{- |
Convert an Either String MultiDegreePixelValues  to an Either String TriCad.MathPolar.MultiDegreeRadii.
Pass in a function, to do the edge detection.
-}

multiDegreePixelValuesToMultiDegreeRadii :: Name -> ([PA.PixelValue] -> Radius) -> Either String PA.MultiDegreePixelValues -> Either String MultiDegreeRadii
multiDegreePixelValuesToMultiDegreeRadii _ _ (Left msg) = Left msg
multiDegreePixelValuesToMultiDegreeRadii scanName edgeDetector (Right (PA.MultiDegreePixelValues pixelValues)) =
  let pixelValuesToRadii singleDegreePixelValues = SingleDegreeRadii {degree=(PA.degree singleDegreePixelValues), radii=[ edgeDetector x   | x  <-  PA.radii singleDegreePixelValues]}
  in  Right (MultiDegreeRadii {name=scanName, degrees=(map (pixelValuesToRadii ) pixelValues)})



