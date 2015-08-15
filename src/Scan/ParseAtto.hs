{-# LANGUAGE OverloadedStrings #-}
module Scan.ParseAtto(getPixelRow, getPixelRowMulti, getDegree, getDegreeScan,
                      getMultiDegreeScan, RawScan(..), RawSingleDegreeScan(..)) where
import Data.Word
import Data.Attoparsec.Char8
import Control.Applicative
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BS

{----------------------------------------- overview-----------------------------------------------------------
Parse a raw data file, from the opencv scanning code.
It only gets the existing values from the raw data.
It does not do any trasformations, such as reducing image row values down to a single measurement, reduce the number of
rows, or anything else. That is left up to the Scan.Transform module, which will use ths RawScan datatype, to create a
MathPolar.Scan datatype, of the reduced/transformed data.
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
data RawScan = RawScan {name::String, degrees::[RawSingleDegreeScan]}
          deriving (Show, Eq)

data RawSingleDegreeScan = RawSingleDegreeScan {degree::Double, radii::[[Double]]}
     deriving (Show, Eq)

{-
--The name assoc'd with the scan.
Having it in the raw data, will mean having a command line arg, in the opencv scanner.
Perhaps it will be better to add the name later, during the Haskell processing.
Leave it in for now, but probably remove before changing the opencv server to produce the raw data for parseAtto.
-}
newtype ScanName = ScanName {scanName::String}

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

{-This is what degree, out of possible 360 degrees, is currently being scanned.-}
getDegree :: Parser Double
getDegree = do
  degree <- double
  return degree

{-
Process an entire 'Single' degree.
-}
getDegreeScan :: Parser RawSingleDegreeScan
getDegreeScan = do
  degree <- getDegree
  radii <- getPixelRowMulti
  return $ RawSingleDegreeScan degree radii

{-
Remove before doing opencv server.
-}
getName :: Parser ScanName
getName = do
  name <- Data.Attoparsec.Char8.takeWhile (/=' ')
  return $ ScanName $ BS.unpack  name

{-
Need to remove name.

This is the final big function, that processes the entire raw data file into a RawScan datatype.
This in turn, will be transformed into a TriCad.MathPolar.Scan datatype using Scan.Transform, during
the process of reducing each pixel data row, down to a single Radius, as well as possibly reducing the number of rows.
-}
getMultiDegreeScan :: Parser RawScan
getMultiDegreeScan = do
  scanName' <- getName
  degreeScans <- sepBy  getDegreeScan (char '$')
  return RawScan {name=(scanName scanName'), degrees=degreeScans}
