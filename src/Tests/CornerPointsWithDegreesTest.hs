module Tests.CornerPointsWithDegreesTest where
import Test.HUnit
import CornerPoints.CornerPointsWithDegrees(CornerPointsWithDegrees(..), (+++~), (+++~>), (|+++~|), (@~+++#@), DegreeRange(..), cubesWithinDegreeRange )
import CornerPoints.CornerPoints(CornerPoints(..),(@+++#@),(+++), (+++>))
import CornerPoints.Points (Point(..))
import CornerPoints.Transpose (transposeZ)
import CornerPoints.FaceConversions(backFaceFromFrontFace, upperFaceFromLowerFace, lowerFaceFromUpperFace )
import  CornerPoints.FaceExtraction (
 extractFrontFace,
 extractBottomFrontLine,
 extractFrontTopLine,
 extractTopFace,
 extractBackTopLine,
 extractBottomFace,
 extractBackBottomLine,
 extractBackFace
 )

testCube = (BottomFace
              {f1 = Point {x_axis = 0.0, y_axis = 1.0, z_axis = 0.0}, f4 = Point {x_axis = 1.0, y_axis = 1.0, z_axis = 0.0},
               b1 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 0.0}, b4 = Point {x_axis = 1.0, y_axis = 0.0, z_axis = 0.0}}
           )
           @+++#@
           ((transposeZ(+1)) . upperFaceFromLowerFace)

testRightFace = 
           (RightFace (Point 1 0 1) (Point 1 0 0) (Point 1 1 1) (Point 1 1 0))

testLeftFace = 
           (LeftFace  (Point 0 0 0)  (Point 0 0 1)  (Point 0 1 0) (Point 0 1 1) )
           
cornerPointsWithDegreesTest = do

  {-Try out the (@~+++#@) by adding a frontFace to a Cube-}
  let transposeACornerPointsWithDegreesUpwards = TestCase $ assertEqual
       "transposeACornerPointsWithDegreesUpwards"
       (CubesWithStartEndDegrees
        {_cube = CubePoints {f1 = Point {x_axis = 0.0, y_axis = 1.0, z_axis = 1.0},
                             f2 = Point {x_axis = 0.0, y_axis = 1.0, z_axis = 2.0},
                             f3 = Point {x_axis = 1.0, y_axis = 1.0, z_axis = 2.0},
                             f4 = Point {x_axis = 1.0, y_axis = 1.0, z_axis = 1.0},
                             b1 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 1.0},
                             b2 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 2.0},
                             b3 = Point {x_axis = 1.0, y_axis = 0.0, z_axis = 2.0},
                             b4 = Point {x_axis = 1.0, y_axis = 0.0, z_axis = 1.0}}, _degreeRange = DegreeRange 0.0 10.0})
       ( (CubesWithStartEndDegrees testCube (DegreeRange 0 10)) @~+++#@ ((transposeZ (+1)). extractTopFace)
       
       )
  runTestTT transposeACornerPointsWithDegreesUpwards


  {-Push a RightFaceWithDegrees into a [LeftFaceWithDegrees]-}
  let pushRightFaceWithDegreesIntoAListOfLeftFaceWithDegrees = TestCase $ assertEqual
        "pushRightFaceWithDegreesIntoAListOfLeftFaceWithDegrees"
        [CubesWithStartEndDegrees
           {_cube = CubePoints
                      {f1 = Point {x_axis = 0.0, y_axis = -2.0, z_axis = 9.0},
                       f2 = Point {x_axis = 0.0, y_axis = -1.0, z_axis = 10.0},
                       f3 = Point {x_axis = 0.0, y_axis = -1.0, z_axis = 10.0},
                       f4 = Point {x_axis = 0.0, y_axis = -2.0, z_axis = 9.0}, 
                       b1 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 9.0},
                       b2 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 10.0},
                       b3 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 10.0},
                       b4 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 9.0}
                      },
              _degreeRange = DegreeRange 0.0 10.0},
         CubesWithStartEndDegrees 
           {_cube = CubePoints {f1 = Point {x_axis = 0.0, y_axis = -2.0, z_axis = 8.0},
                       f2 = Point {x_axis = 0.0, y_axis = -2.0, z_axis = 9.0},
                       f3 = Point {x_axis = 0.0, y_axis = -1.0, z_axis = 10.0},
                       f4 = Point {x_axis = 0.0, y_axis = -2.0, z_axis = 9.0},
                       b1 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 8.0},
                       b2 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 9.0},
                       b3 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 10.0},
                       b4 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 9.0}},
              _degreeRange = DegreeRange 10.0 20.0}
        ]
        (let listOfLeftFacesWithDegrees =
                [LeftFaceWithDegrees
                  {_leftFace = LeftFace {b2 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 10.0}, b1 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 9.0},
                                         f2 = Point {x_axis = 0.0, y_axis = -1.0, z_axis = 10.0}, f1 = Point {x_axis = 0.0, y_axis = -2.0, z_axis = 9.0}},
                  _degree = 10.0},
                 LeftFaceWithDegrees
                  {_leftFace = LeftFace {b2 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 9.0}, b1 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 8.0},
                              f2 = Point {x_axis = 0.0, y_axis = -2.0, z_axis = 9.0}, f1 = Point {x_axis = 0.0, y_axis = -2.0, z_axis = 8.0}},
                   _degree = 20.0}
                ]
  
             rightFaceWithDegrees =
                RightFaceWithDegrees
                  {_rightFace = RightFace {b3 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 10.0}, b4 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 9.0},
                                           f3 = Point {x_axis = 0.0, y_axis = -1.0, z_axis = 10.0}, f4 = Point {x_axis = 0.0, y_axis = -2.0, z_axis = 9.0}},
                   _degree = 0.0}
  
         in rightFaceWithDegrees +++~> listOfLeftFacesWithDegrees
           
        )
  runTestTT pushRightFaceWithDegreesIntoAListOfLeftFaceWithDegrees
  
  
  
  
  
    
  {-Create a [CubesWithStartEndDegrees] from FaceWithDegrees, then filter them-}
  let createCubesWithStartEndDegreesFromFaceWithDegrees = TestCase $ assertEqual
        "createCubesWithStartEndDegreesFromFaceWithDegrees"
        [
           CubesWithStartEndDegrees testCube (DegreeRange 90 180) ,
           CubesWithStartEndDegrees testCube (DegreeRange 180 270)
        ]
        (cubesWithinDegreeRange (DegreeRange 90 270)
         [
           (RightFaceWithDegrees testRightFace 0) +++~ (LeftFaceWithDegrees testLeftFace 90),
           (RightFaceWithDegrees testRightFace 90) +++~ (LeftFaceWithDegrees testLeftFace 180),
           (RightFaceWithDegrees testRightFace 180) +++~ (LeftFaceWithDegrees testLeftFace 270),
           (RightFaceWithDegrees testRightFace 270) +++~ (LeftFaceWithDegrees testLeftFace 360)
         ]
        )
  runTestTT createCubesWithStartEndDegreesFromFaceWithDegrees
  

  

  
  {-
  Create a map of CubesWithStartEndDegrees.
  See if it is in range.
  Test a 
  -}
  let  
       cubesWithStartEndDegreesTest = TestCase $ assertEqual
        "CubesWithStartEndDegrees"
        [
           CubesWithStartEndDegrees testCube (DegreeRange 90 180) ,
           CubesWithStartEndDegrees testCube (DegreeRange 180 270)
        ]
        (cubesWithinDegreeRange (DegreeRange 90 270)
         [
           CubesWithStartEndDegrees testCube (DegreeRange 0 90) ,
           CubesWithStartEndDegrees testCube (DegreeRange 90 180) ,
           CubesWithStartEndDegrees testCube (DegreeRange 180 270) ,
           CubesWithStartEndDegrees testCube (DegreeRange 270 360) 
         ]
        )
  runTestTT cubesWithStartEndDegreesTest

  let addLeftRigtFacesTest = TestCase $ assertEqual
        "addLeftRigtFacesTest"
        (CubesWithStartEndDegrees (testRightFace +++ testLeftFace) (DegreeRange 0 90))
        (
           (RightFaceWithDegrees testRightFace 0) +++~ (LeftFaceWithDegrees testLeftFace 90)
        )
  runTestTT addLeftRigtFacesTest



