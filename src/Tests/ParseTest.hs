module Tests.ParseTest (parseTestDo) where
import Test.HUnit
import Scan.Parse( readDouble)

parseTestDo = do
  runTestTT readDoubleTest

readDoubleTest = TestCase $ assertEqual
 "read in a double"
 (1)
 (readDouble "1")
