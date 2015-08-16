module Tests.TransformTest() where
import Test.HUnit
import Scan.Transform(minValueIndices, average, reduceRows, reduceScanRows)
import TriCad.MathPolar( Radius(..), Scan(..), SingleDegreeScan(..))
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString as B
import qualified  Data.ByteString.Char8 as BC (pack) 
import GHC.Word (Word8)
import qualified  Data.ByteString.Internal as BI (unpackBytes)
import Data.Attoparsec.Char8
import Scan.ParseAtto(getPixelRow, getPixelRowMulti, getDegree, getRawDegreeScan, RawSingleDegreeScan(..),
                      getRawMultiDegreeScan, RawScan(..), rawScanToScan, )


transformTest = do
  runTestTT getArrayOfMinValuePositions
  runTestTT getArrayOfAvgMinValuePositions
  runTestTT reduceRowsSimpleTest
  --transform MathPolar.Scan
  runTestTT reduceScanRowsTest
  runTestTT reduceScanRowsBy0Factor
  runTestTT reduceScanRowsByToLargeAFactor
  
--create a [Word8] for: Right(B.pack $ strToWord8s)
--which gets a bytstring of word8
strToWord8s :: String -> [Word8]
strToWord8s = BI.unpackBytes . BC.pack

---------------------------------------  transforming the data.----------------------------
getArrayOfMinValuePositions = TestCase $ assertEqual 
 "reduce the array down to a set of values that are less than a min val."
 ([2,3,7])
 ( let arrayOfInt = [9, 9, 2, 4, 5, 7, 9, 2]
   in minValueIndices 4 arrayOfInt
 )



getArrayOfAvgMinValuePositions = TestCase $ assertEqual
 "Get the average min value positions"
 (4.0)
 (let arrayOfInt :: [Double]
      arrayOfInt = [9, 9, 2, 4, 5, 7, 9, 2]
  in  average $ minValueIndices 4 arrayOfInt
 )


reduceRowsSimpleTest = TestCase $ assertEqual
  "Reduce the number of rows, based on a filter factor, of a simple list"
  ( [3.0,6.0])
  (reduceRows 3 [1,2,3,4,5,6,7] )

----------------------------------------- working with MathPolar.Scan ----------------------------------

reduceScanRowsTest = TestCase $ assertEqual
  "get a Scan from a RawScan"
  (Right(Scan {name = "myScan", degrees = [SingleDegreeScan {degree = 1.0, radii = [Radius {radius = 0.5}]},
                                     SingleDegreeScan {degree = 2.0, radii = [Radius {radius = 0.0}]}]}))
  ( let rawScan = (Right (B.pack $strToWord8s "1 1 2 3;1 2 3$2 1 2 3;1 3 3")  >>=  parseOnly  getRawMultiDegreeScan)
        scan = rawScanToScan  "myScan" (average . minValueIndices 2) rawScan
    in  scan >>= reduceScanRows 2 
  )


reduceScanRowsBy0Factor = TestCase $ assertEqual
  "get a Scan from a RawScan"
  (Left "Can't use 0 for row reduction")
  ( let rawScan = (Right (B.pack $strToWord8s "1 1 2 3;1 2 3$2 1 2 3;1 3 3")  >>=  parseOnly  getRawMultiDegreeScan)
        scan = rawScanToScan  "myScan" (average . minValueIndices 2) rawScan
    in  scan >>= reduceScanRows 0
  )

reduceScanRowsByToLargeAFactor = TestCase $ assertEqual
  "get a Scan from a RawScan"
  --(Right (Scan {name = "myScan", degrees = [SingleDegreeScan {degree = 1.0, radii = []},SingleDegreeScan {degree = 2.0, radii = []}]}))
  (Left  "reduction factor > number of rows")
  ( let rawScan = (Right (B.pack $strToWord8s "1 1 2 3;1 2 3$2 1 2 3;1 3 3")  >>=  parseOnly  getRawMultiDegreeScan)
        scan = rawScanToScan  "myScan" (average . minValueIndices 2) rawScan
    in  scan >>= reduceScanRows 3
  )
