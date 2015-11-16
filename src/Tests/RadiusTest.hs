
module Tests.RadiusTest(radisuTestDo) where
import Test.HUnit
import CornerPoints.Radius(Radius(..), SingleDegreeRadii(..), Degree(..), MultiDegreeRadii(..),
                          extractSingle, extractList, rotateMDR, sortMDR)
import CornerPoints.Transposable(transpose)


radisuTestDo = do
 runTestTT extractRadiusFromMultiDegreeRadiiTest
 runTestTT extractRadiusFromMultiDegreeRadiiTest2
 runTestTT extractRadiusFromSinleDegreeRadiiTest
 runTestTT extractRadiusFromSinleDegreeRadiiTest2

 runTestTT transposeRadiusTest
 runTestTT transposeSDRTest
 runTestTT transposeSDRTest2

 runTestTT rotateMultiDegreeRadiiTest

 runTestTT sortMultiDegreeRadiiTest
 runTestTT sortMultiDegreeRadiiOverlappingTest



sortMultiDegreeRadiiTest = TestCase $ assertEqual 
  "rotateMultiDegreeRadiiTest"
  (MultiDegreeRadii "name"                   [SingleDegreeRadii 1 [Radius 1], SingleDegreeRadii 10 [Radius 1], SingleDegreeRadii 20 [Radius 1]])
  (sortMDR $ MultiDegreeRadii "name"  [SingleDegreeRadii 10 [Radius 1], SingleDegreeRadii 1 [Radius 1], SingleDegreeRadii 20 [Radius 1]])

sortMultiDegreeRadiiOverlappingTest = TestCase $ assertEqual 
  "sortMultiDegreeRadiiOverlappingTest"
  (MultiDegreeRadii "name"                   [SingleDegreeRadii 10 [Radius 1], SingleDegreeRadii 10 [Radius 1], SingleDegreeRadii 20 [Radius 1]])
  (sortMDR $ MultiDegreeRadii "name"  [SingleDegreeRadii 10 [Radius 1], SingleDegreeRadii 10 [Radius 1], SingleDegreeRadii 20 [Radius 1]])

rotateMultiDegreeRadiiTest = TestCase $ assertEqual 
  "rotateMultiDegreeRadiiTest"
  (MultiDegreeRadii "name" [SingleDegreeRadii 0 [Radius 20], SingleDegreeRadii 10 [Radius 0], SingleDegreeRadii 20 [Radius 10]])
  (rotateMDR 5 (MultiDegreeRadii "name" [SingleDegreeRadii 0 [Radius 0], SingleDegreeRadii 10 [Radius 10], SingleDegreeRadii 20 [Radius 20]]))

transposeRadiusTest = TestCase $ assertEqual
  "transposeRadiusTest"
  (Radius 4)
  (transpose (+3) (Radius 1))

transposeSDRTest = TestCase $ assertEqual
  "transposeSDRTest"
  (SingleDegreeRadii 1 [ Radius 5])
  (transpose (+3) (SingleDegreeRadii 1 [Radius 2]))
  
transposeSDRTest2 = TestCase $ assertEqual
  "transposeSDRTest2"
  (SingleDegreeRadii 1 [Radius 4, Radius 5])
  (transpose (+2) (SingleDegreeRadii 1 [Radius 2, Radius 3])  )
  

extractRadiusFromMultiDegreeRadiiTest = TestCase $ assertEqual
  "extractRadiusFromMultiDegreeRadiiTest"
  (MultiDegreeRadii "name" [SingleDegreeRadii 1 [Radius 1]])
  (extractSingle head (MultiDegreeRadii "name" [SingleDegreeRadii 1 [Radius 1]]))

extractRadiusFromMultiDegreeRadiiTest2 = TestCase $ assertEqual
  "extractRadiusFromMultiDegreeRadiiTest2"
  (MultiDegreeRadii "name" [SingleDegreeRadii 1 [Radius 2, Radius 3]])
  (extractList tail (MultiDegreeRadii "name" [SingleDegreeRadii 1 [Radius 1, Radius 2, Radius 3]]))
  
extractRadiusFromSinleDegreeRadiiTest = TestCase $ assertEqual
  "extractRadiusFromSinleDegreeRadiiTest"
  (SingleDegreeRadii 1 [Radius 1])
  (extractSingle head (SingleDegreeRadii 1 [Radius 1]))

extractRadiusFromSinleDegreeRadiiTest2 = TestCase $ assertEqual
  "extractRadiusFromSinleDegreeRadiiTest2"
  (SingleDegreeRadii 1 [Radius 2, Radius 3])
  (extractList tail (SingleDegreeRadii 1 [Radius 1, Radius 2, Radius 3]))


