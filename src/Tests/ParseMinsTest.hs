module Tests.ParseMinsTest(parseMinsDo) where
import Test.HUnit
import Scan.Parse.Mins(parseMinsToChar, parseMinsToDouble, parseMinsToRadius)
import qualified Data.ByteString.Lazy.Char8 as BL
import TriCad.MathPolar( Radius(..))

parseMinsDo = do
  runTestTT hello
  runTestTT parseMinsToDoubleTest
  runTestTT parseMinsToRadiusTest

hello = TestCase $ assertEqual
  "hello from parse mins"
  (1)
  (1)
  
parseMinsToCharTest = TestCase $ assertEqual
  "parse to a list of char"
  ([["1","2"],["3","4"]])
  (parseMinsToChar $ BL.pack "1 2;3 4")

parseMinsToDoubleTest = TestCase $ assertEqual
  "parse to a list of char"
  ([[1,2],[3,4]])
  (parseMinsToDouble $ BL.pack "1 2;3 4")

parseMinsToRadiusTest = TestCase $ assertEqual
  "parse to a list of char"
  ([[Radius(1),Radius(2)],[Radius(3),Radius(4)]])
  (parseMinsToRadius $ BL.pack "1 2;3 4")
