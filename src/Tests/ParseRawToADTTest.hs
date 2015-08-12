{-# LANGUAGE OverloadedStrings #-}
--{-# LANGUAGE OverloadedStrings #-}
module Tests.ParseRawToADTTest(parseRawToADTTestDo)  where
import Test.HUnit
import Scan.Parse.RawToADT(parseToScan)
import qualified Data.ByteString.Lazy.Char8 as BL
import Scan.Transform(minValueIndices, average)
import TriCad.MathPolar( Radius(..),Degree(..), Scan(..))

parseRawToADTTestDo = do
  runTestTT parseToScanTest

hello = TestCase $ assertEqual
  "parseTest"
  (1)
  (1)


parseToScanTest = TestCase $ assertEqual
  "parseTest"
  (Scan {name="myScan", degrees=([(Degree{degree=0,radii=[Radius 0.5, Radius 1.0]}),(Degree{degree=90,radii=[Radius 0.5, Radius 0.5]}) ])})
  (parseToScan (average . minValueIndices 5 ) "0 1 3 6;0 3 4 5 7$90 2 4 6;90 3 4 8")



