module Tests.ParseMinsTest(parseMinsDo) where
import Test.HUnit
import Scan.Parse.Mins(parseToChar, parseToDouble, parseToRadius)
import qualified Data.ByteString.Lazy.Char8 as BL
import TriCad.MathPolar( Radius(..))

parseMinsDo = do
  runTestTT parseMinsToDoubleTest
  runTestTT parseMinsToRadiusTest

  
parseMinsToCharTest = TestCase $ assertEqual
  "parse to a list of char"
  ([["1","2"],["3","4"]])
  (parseToChar $ BL.pack "1 2;3 4")

parseMinsToDoubleTest = TestCase $ assertEqual
  "parse to a list of char"
  ([[1,2],[3,4]])
  (parseToDouble $ BL.pack "1 2;3 4")

parseMinsToRadiusTest = TestCase $ assertEqual
  "parse to a list of char"
  ([[Radius(1),Radius(2)],[Radius(3),Radius(4)]])
  (parseToRadius $ BL.pack "1 2;3 4")
