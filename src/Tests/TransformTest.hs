module Tests.TransformTest() where
import Test.HUnit
import Scan.Transform(minValueIndices, average, reduceRows)
import TriCad.MathPolar( Radius(..))
import Scan.Parse.Raw(parseToRadiusFiltered)
import qualified Data.ByteString.Lazy.Char8 as BL



transformTest = do
  runTestTT getArrayOfMinValuePositions
  runTestTT getArrayOfAvgMinValuePositions
  runTestTT reduceRowsSimpleTest
  runTestTT reduceRowsTest

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


reduceRowsTest =  TestCase $ assertEqual
  "reduce the number of rows, of actual raw data"
  
  ( [[Radius {radius = 0.5},Radius {radius = 0.5}]])
  ( reduceRows 2 $ parseToRadiusFiltered (average . minValueIndices 5 ) $ BL.pack  "1 3 6;3 4 5 7$2 4 6;3 4 8"
    
  )
