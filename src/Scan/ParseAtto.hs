{-# LANGUAGE OverloadedStrings #-}
module Scan.ParseAtto(getPixelRow, getPixelRowMulti, getDegree, getRawDegreeScan,
                      getRawMultiDegreeScan, PixelValueScan(..), PixelValueSingleDegreeScan(..), rawScanToScan,
                      ) where
import Data.Word
import Data.Attoparsec.Char8
import Control.Applicative
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BS
import Scan.Transform(minValueIndices, average)
import TriCad.MathPolar(Scan(..),SingleDegreeScan(..), Radius(..), Degree)
import TriCad.Types(PixelValue(..))

{----------------------------------------- overview-----------------------------------------------------------
Parse a raw data file, from the opencv scanning code.
It only gets the existing values from the raw data.
It does not do any trasformations, such as reducing image row values down to a single measurement, reduce the number of
rows, or anything else. That is left up to the Scan.Transform module, which will use ths RawScan datatype, to create a
MathPolar.Scan datatype, of the reduced/transformed data.
-}

{-
Parse a raw file to an Either String Scan.ParseAtto.RawScan.
-}

--used to remvove leading/trailing spaces.
--need testing in place, then see if '\t' can be removed.
spaceSkip = many $ satisfy $ inClass [ ' ' , '\t' ]

{-
These data types parallel TriCad.MathPolar Scan and SingleDegreeScan but with the differences:
-The Radius type is left as a Double, for easier use in the Scan.Transform module.
-The RawSingleDegreeScan.radii is [[Double]] instead of [Radius], because the row has not yet been
 reduced to a single value, representing a Radius. This is done later, during the transform stage of the parse.
-}
data PixelValueScan = PixelValueScan {rawDegrees::[PixelValueSingleDegreeScan]}
          deriving (Show, Eq)



data PixelValueSingleDegreeScan = PixelValueSingleDegreeScan {rawDegree::Degree, rawRadii::[[PixelValue]]}
     deriving (Show, Eq)

              
{-----------------------------------------------------------------------------------------
Convert a Either String RawScan to an Either String Scan

Why:
A raw scan is just a copy of the raw image data, parsed by attoparsec and put into a datatype.
It needs the image row data filtered down to a single value (Radius) for each row of each image.
It needs this reduced row info, along with a name, put into a TriCad.MathPolar.Scan to be used by TriCad.

If there is an error by attoparsec, during parsing, this will be passed on as a Left Scan, so
that  Either String Scan monad can be used for further work on the data.
-}

{-
Reduce the rows of pixel data for a single degree of raw data, down to a [TriCad.MathPolar.Radius]
so that there is a vertical line of Radius.
-}
reduceRadii :: ([PixelValue] -> Radius) -> [PixelValue] -> Radius
reduceRadii f inRadii = f inRadii
{-
reduceRadii :: ([Double] -> Radius) -> [Double] -> Radius
reduceRadii f inRadii =  Radius $  f inRadii
-}

{-Take the reduced raw pixel data from 'reduceRadii', add the current degree val, and put into a TriCad.MathPolar.RawSingleDegreeScan.
Doing the to every RawSingleDegreeScan will result in the final TriCad.MathPolar.Scan datatype, along with the name.-}
reduceDegreeScan :: ([PixelValue] -> Radius) -> PixelValueSingleDegreeScan -> SingleDegreeScan
reduceDegreeScan f inRawDegree = SingleDegreeScan {degree=(rawDegree inRawDegree), radii=[ reduceRadii f x   | x  <-  rawRadii inRawDegree]}
{-
Convert an Either String Scan.ParseAtto.RawScan.  to a Either String TriCad.MathPolar.Scan datatype, so that
further transformations can be done to it.
-}
rawScanToScan :: String -> ([PixelValue] -> Radius) -> Either String PixelValueScan -> Either String Scan
rawScanToScan _ _ (Left msg) = Left msg
rawScanToScan scanName f (Right (PixelValueScan inRawDegrees)) = Right (Scan {name=scanName, degrees=(map (reduceDegreeScan f) inRawDegrees)})

--------------------------------------------- end: convert RawScan to Scan---------------------------------------------------
----------------------------------------------------------------------------------------------------------------------------


---------------------------------------------- attoparsec --------------------------------------------------------------------


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
getRawDegreeScan :: Parser PixelValueSingleDegreeScan
getRawDegreeScan = do
  degree <- getDegree
  radii <- getPixelRowMulti
  return $ PixelValueSingleDegreeScan degree radii


{-
Need to remove name.

This is the final big function, that processes the entire raw data file into a RawScan datatype.
This in turn, will be transformed into a TriCad.MathPolar.Scan datatype using Scan.Transform, during
the process of reducing each pixel data row, down to a single Radius, as well as possibly reducing the number of rows.
-}
getRawMultiDegreeScan :: Parser PixelValueScan
getRawMultiDegreeScan = do
  degreeScans <- sepBy  getRawDegreeScan (char '$')
  return PixelValueScan {rawDegrees=degreeScans}




{-
No longer used.
Leave it here for now, as it is a nice example of how to read a string terminated by a space.

getName :: Parser ScanName
getName = do
  name <- Data.Attoparsec.Char8.takeWhile (/=' ')
  return $ ScanName $ BS.unpack  name
-}
