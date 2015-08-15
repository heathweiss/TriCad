module Tests.ParseAttoTest where
import Test.HUnit
import Scan.ParseAtto(getPixelRow, getPixelRowMulti, getDegree, getDegreeScan, DegreeScan(..),
                      getMultiDegreeScan, MultiDegreeScan(..))
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Control.Applicative
import Data.Char
import Data.Word
import Data.Attoparsec.Char8
import Control.Applicative

import qualified  Data.ByteString.Internal as BI (unpackBytes)
import qualified  Data.ByteString.Char8 as BC (pack) 
import GHC.Word (Word8)
import Scan.Transform(minValueIndices, average, reduceRows)

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
  (1.0)
  ( let parseResults =  (Right(B.pack $ strToWord8s "10 20 30 40")  >>=  parseOnly  getPixelRow)
    in 
        case parseResults of
         Right ints -> average $ minValueIndices 30 ints
        
  )

readMultiRowsOfIntsAndReduceToAverageMinVals = TestCase $ assertEqual
 "read multi rows of ints, and reduce them down to average index of min values."
 ([0.5,0.5])
 ( let parseResults = (Right (B.pack $strToWord8s "10 20 30 40;10 20 30 50")  >>=  parseOnly  getPixelRowMulti)
       
   in  case parseResults of
        Right rowsOfInts -> map (average . minValueIndices 20 ) rowsOfInts
 )


readMultiRowsOfIntsReduceRowsAndReduceToAverageMinVals = TestCase $ assertEqual
 "read multi rows of ints, reduce the # of rows, and then reduce them down to average index of min values."
 ([0.5])
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
  (Right (DegreeScan {degree=1, radii= [[10.0,20.0],[10.0,20.0]]}) )
  (Right (B.pack $strToWord8s "1 10 20;10 20")  >>=  parseOnly  getDegreeScan)

getACompleteScan = TestCase $ assertEqual
  "get a complete scan"
  (Right(MultiDegreeScan {name="myScan", degrees=[(DegreeScan {degree=1, radii= [[10.0,20.0],[10.0,20.0]]})]}))
  (Right (B.pack $strToWord8s "myScan 1 10 20;10 20$2 1 2;3 4")  >>=  parseOnly  getMultiDegreeScan)
