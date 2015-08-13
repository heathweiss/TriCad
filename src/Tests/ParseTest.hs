{-# LANGUAGE OverloadedStrings #-}
module Tests.ParseTest (parseTestDo) where
import Test.HUnit
import Scan.Parse( readDouble, parseToScan)
import qualified Data.ByteString.Lazy.Char8 as BL
import Scan.Transform(minValueIndices, average)
import TriCad.MathPolar( Radius(..),Degree(..), Scan(..))

parseTestDo = do
  runTestTT readDoubleTest
  runTestTT parseToScanTest

readDoubleTest = TestCase $ assertEqual
 "read in a double"
 (1)
 (readDouble "1")

parseToScanTest = TestCase $ assertEqual
  "parseTest"
  (Just (Scan {name="myScan", degrees=([(Degree{degree=0,radii=[Radius 0.5, Radius 1.0]}),(Degree{degree=90,radii=[Radius 0.5, Radius 0.5]}) ])}))
  (parseToScan (average . minValueIndices 5 ) "0 1 3 6;0 3 4 5 7$90 2 4 6;90 3 4 8")
