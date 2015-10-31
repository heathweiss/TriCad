{-# LANGUAGE OverloadedStrings #-}

{- |

Module            : Scan.ParseAtto
Description       : Uses Attoparsec to parse raw image data

Parse the raw csv data, into a datatype.
This raw data file,  is a camera image processed by the opencv scanning code.

-}

module Scan.Parse(MultiDegreePixelValues(..), SingleDegreePixelValues(..), PixelValue(..), parseCSVPixelValues) where
import Data.Word
--import Data.Attoparsec.Char8
import Data.Attoparsec.ByteString.Char8
import Control.Applicative
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BS
import qualified CornerPoints.Create as MP (Degree)
import CornerPoints.VerticalFaces(SingleDegreeRadii(..))
--import CornerPoints.Radius(Radius(..))

--used to remvove leading/trailing spaces.
--need testing in place, then see if '\t' can be removed.
spaceSkip = many $ satisfy $ inClass [ ' ' , '\t' ]

{- |
All the pixel values captured in all the camera images, for complete scan.
-}
data MultiDegreePixelValues = MultiDegreePixelValues {degrees::[SingleDegreePixelValues]}
          deriving (Show, Eq)


-- |All the pixel values as captured by camera, for a single image, which represents a single degree of a scan.
data SingleDegreePixelValues = SingleDegreePixelValues {degree::MP.Degree, radii::[[PixelValue]]}
     deriving (Show, Eq)


-- |Actual pixel value, as captured by the scanner/camera.
type PixelValue = Double

              

--------------------------------------------- end: convert RawScan to Scan---------------------------------------------------
----------------------------------------------------------------------------------------------------------------------------


---------------------------------------------- attoparsec --------------------------------------------------------------------
-- | Uses attoparsec parseOnly to parse the raw image data from the scan, using internal parse functions based on attoparsec.
parseCSVPixelValues = parseOnly getRawMultiDegreeScan

{-The row data come in the form of Double,space,Double...
This processes a single Double,space combination.
-}
getPixelRowBase :: Parser Double
getPixelRowBase = do
  spaceSkip
  pix1 <- double
  return $ pix1

{-Converts an entire row of Double,space... into a list of double.
This is raw data row of the pixel values.-}
getPixelRow :: Parser [Double]
getPixelRow = do
  many $ getPixelRowBase

{-Process all the image rows of pixel data, into [[Double]]-}
getPixelRowMulti :: Parser [[Double]]
getPixelRowMulti = do
  sepBy getPixelRow (char ';')

{-This is what degree, out of possible 360 degrees, is currently being scanned.
This is stored as the 1st double of the first row of each scan.
-}
getDegree :: Parser Double
getDegree = do
  degree <- double
  return degree

{-
Process an entire 'Single' degree scan.
The 1st double of 1st row will be the degree.
All subsequent values will be image pixel values.
-}
getRawDegreeScan :: Parser SingleDegreePixelValues
getRawDegreeScan = do
  degree <- getDegree
  radii <- getPixelRowMulti
  return $ SingleDegreePixelValues degree radii


{-
This is the final big function, that processes the entire raw data file into a RawScan datatype.
This in turn, will be transformed into a TriCad.MathPolar.Scan datatype using Scan.Transform, during
the process of reducing each pixel data row, down to a single Radius, as well as possibly reducing the number of rows.
-}
getRawMultiDegreeScan :: Parser MultiDegreePixelValues
getRawMultiDegreeScan = do
  degreeScans <- sepBy  getRawDegreeScan (char '$')
  return MultiDegreePixelValues {degrees=degreeScans}




{-
No longer used.
Leave it here for now, as it is a nice example of how to read a string terminated by a space.

getName :: Parser ScanName
getName = do
  name <- Data.Attoparsec.Char8.takeWhile (/=' ')
  return $ ScanName $ BS.unpack  name
-}
