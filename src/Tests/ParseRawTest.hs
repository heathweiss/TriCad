module Tests.ParseRawTest (parseRawDo) where
import Test.HUnit
import Scan.Parse.Raw(parseToChar, parseToDouble, parseToDoubleFiltered, parseToDoubleFilteredRadius)
import qualified Data.ByteString.Lazy.Char8 as BL
import Scan.Transform(minValueIndices, average)
import TriCad.MathPolar( Radius(..))

parseRawDo = do
  runTestTT parseToCharTest
  runTestTT parseToDoubleTest
  runTestTT parseToDoubleFilteredTest
  runTestTT parseToDoubleFilteredRadiusTest

parseToCharTest = TestCase $ assertEqual
  "parse to char"
  ([[["1","2"],["3","4"]],[["5","6"],["7","8"]]])
  (parseToChar $ BL.pack  "1 2;3 4$5 6;7 8")

parseToDoubleTest = TestCase $ assertEqual
  "parse to double"
  
  ([[[1.0,2.0],[3.0,4.0]],[[5.0,6.0],[7.0,8.0]]])
  (parseToDouble $ BL.pack  "1 2;3 4$5 6;7 8")


parseToDoubleFilteredTest = TestCase $ assertEqual
  "parse to average indice of all values <= 5"
  
  ( [[0.5,1.0],[0.5,0.5]])
  ( parseToDoubleFiltered (average . minValueIndices 5 ) $ BL.pack  "1 3 6;3 4 5 7$2 4 6;3 4 8"
    
  )

parseToDoubleFilteredRadiusTest = TestCase $ assertEqual
  "parse to average indice of all values <= 5, and put them in a Radius"
  
  ( [[Radius {radius = 0.5},Radius {radius = 1.0}],[Radius {radius = 0.5},Radius {radius = 0.5}]])
  ( parseToDoubleFilteredRadius (average . minValueIndices 5 ) $ BL.pack  "1 3 6;3 4 5 7$2 4 6;3 4 8"
    
  )
