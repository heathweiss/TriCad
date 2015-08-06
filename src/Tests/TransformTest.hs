module Tests.TransformTest() where
import Test.HUnit
import Scan.Transform(minValueIndices, average)



transformTest = do
  runTestTT getArrayOfMinValuePositions
  runTestTT getArrayOfAvgMinValuePositions

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
