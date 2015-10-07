module Tests.ParseAttoTest(parseAttoTestDo) where
import Test.HUnit
import Scan.ParseAtto(getPixelRow, getPixelRowMulti, getDegree, getRawDegreeScan, PixelValueSingleDegreeScan(..),
                      getRawMultiDegreeScan, PixelValueScan(..), rawScanToScan)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Control.Applicative
import Data.Char
import Data.Word
import Data.Attoparsec.Char8
import Control.Applicative
import TriCad.MathPolar(Radius(..), Scan(..), SingleDegreeScan(..))
import qualified  Data.ByteString.Internal as BI (unpackBytes)
import qualified  Data.ByteString.Char8 as BC (pack) 
import GHC.Word (Word8)
import Scan.Transform(minValueIndices, average, reduceRows, reduceScanRows)

--create a [Word8] for: Right(B.pack $ strToWord8s)
--which gets a bytstring of word8
strToWord8s :: String -> [Word8]
strToWord8s = BI.unpackBytes . BC.pack

parseAttoTestDo = do
  --radius
  runTestTT readARowOfInts
  runTestTT readMultiRowsOfInts
  runTestTT readRowOfIntsAndReducetoAverageMinVals
  runTestTT readMultiRowsOfIntsAndReduceToAverageMinVals
  runTestTT readMultiRowsOfIntsReduceRowsAndReduceToAverageMinVals

  --degrees
  runTestTT getADegree
  runTestTT getADegreeThenARowOfInts
  runTestTT getACompleteRawScan
  runTestTT getAScanFromARawScan
  runTestTT reduceScanRowsTest

readARowOfInts = TestCase $ assertEqual
  "read a list of ints"
  (Right[10,20, 30,40])
  ((Right (B.pack $strToWord8s "10 20 30 40;50")  >>=  parseOnly  getPixelRow))


readMultiRowsOfInts = TestCase $ assertEqual
  "read a multi lines of ints"
  (Right[[10,20, 30,40],[50, 60]])
  ((Right (B.pack $strToWord8s "10 20 30 40;50 60")  >>=  parseOnly  getPixelRowMulti))

readRowOfIntsAndReducetoAverageMinVals = TestCase $ assertEqual
  "read a single row of Ints, and reduce them down to average index of min vals"
  (Radius 1.0)--(1.0)
  ( let parseResults =  (Right(B.pack $ strToWord8s "10 20 30 40")  >>=  parseOnly  getPixelRow)
    in 
        case parseResults of
         Right ints -> average $ minValueIndices 30 ints
        
  )

readMultiRowsOfIntsAndReduceToAverageMinVals = TestCase $ assertEqual
 "read multi rows of ints, and reduce them down to average index of min values."
 ([(Radius 0.5),(Radius 0.5)])
 ( let parseResults = (Right (B.pack $strToWord8s "10 20 30 40;10 20 30 50")  >>=  parseOnly  getPixelRowMulti)
       
   in  case parseResults of
        Right rowsOfInts -> map (average . minValueIndices 20 ) rowsOfInts
 )


readMultiRowsOfIntsReduceRowsAndReduceToAverageMinVals = TestCase $ assertEqual
 "read multi rows of ints, reduce the # of rows, and then reduce them down to average index of min values."
 ([Radius 0.5])
 ( let parseResults = (Right (B.pack $strToWord8s "10 20 30 40;10 20 30 50")  >>=  parseOnly  getPixelRowMulti)
       
   in  case parseResults of
        Right rowsOfInts -> map (average . minValueIndices 20 ) $ reduceRows 2 rowsOfInts
 )

 
getADegree = TestCase $ assertEqual
  "remove the leading degree"
  (Right 1)
  (Right (B.pack $strToWord8s "1 10 20 30 40;10 20 30 50")  >>=  parseOnly  getDegree)
       
 
getADegreeThenARowOfInts = TestCase $ assertEqual
  "get a degree, then a row of ints"
  (Right (PixelValueSingleDegreeScan {rawDegree=1, rawRadii= [[10.0,20.0],[10.0,20.0]]}) )
  (Right (B.pack $strToWord8s "1 10 20;10 20")  >>=  parseOnly  getRawDegreeScan)

getACompleteRawScan = TestCase $ assertEqual
  "get a complete scan"
  (Right(PixelValueScan {rawDegrees=[(PixelValueSingleDegreeScan {rawDegree=1, rawRadii= [[10.0,20.0],[10.0,20.0]]}),
                              (PixelValueSingleDegreeScan {rawDegree=2, rawRadii= [[1.0,2.0],[3.0,4.0]]})
                             ]}))
  (Right (B.pack $strToWord8s "1 10 20;10 20$2 1 2;3 4")  >>=  parseOnly  getRawMultiDegreeScan)

getAScanFromARawScan = TestCase $ assertEqual
  "get a Scan from a RawScan"
  (Right(Scan {       name="myScan",
                      degrees=[(SingleDegreeScan {degree=1, radii= [Radius 0.5,Radius 0.5]}),
                              (SingleDegreeScan {degree=2, radii= [Radius 0.5,Radius 0.0]})
                             ]}))
  ( let rawScan = (Right (B.pack $strToWord8s "1 1 2 3;1 2 3$2 1 2 3;1 3 3")  >>=  parseOnly  getRawMultiDegreeScan)
    in  rawScanToScan  "myScan" (average . minValueIndices 2) rawScan
  )


{-
create a 2 row raw scan the same as from my scan raw project to see what is going wrong.
-}
reduceScanRowsTest = TestCase $ assertEqual
  "get a Scan from a RawScan and use ReduceScanRows on it"
  
  (Right (Scan {name = "myScan", degrees = [SingleDegreeScan {degree = 0.0, radii = [Radius {radius = 0.5}]},SingleDegreeScan {degree = 90.0, radii = [Radius {radius = 0.5}]},SingleDegreeScan {degree = 180.0, radii = [Radius {radius = 0.5}]},SingleDegreeScan {degree = 270.0, radii = [Radius {radius = 0.5}]},SingleDegreeScan {degree = 360.0, radii = [Radius {radius = 0.5}]}]}))
  
  ( let rawScan = (Right (B.pack $strToWord8s "0 1 2 3;1 2 3;1 2 3$90 1 2 3;1 2 3;1 2 3$180 1 2 3;1 2 3;1 2 3$270 1 2 3;1 2 3;1 2 3$360 1 2 3;1 2 3;1 2 3")  >>=  parseOnly  getRawMultiDegreeScan)
    in  rawScanToScan "myScan" (average . minValueIndices 2) rawScan  >>= reduceScanRows 2
    
  )
