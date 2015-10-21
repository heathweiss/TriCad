module Tests.VerticalFacesTest(verticalFacesTestDo)where

import Test.HUnit
import CornerPoints.VerticalFaces(createRightFaces, createLeftFaces, createHorizontallyAlignedCubes, createLeftFacesMultiColumns,
                                  SingleDegreeRadii(..), MultiDegreeRadii(..), TransposeFactor)
import CornerPoints.Create(Slope(..), Point(..), Origin(..), createCornerPoint, Angle(..), Degree(..), Radius(..), flatXSlope, flatYSlope)
import CornerPoints.CornerPoints(CornerPoints(..))

verticalFacesTestDo = do
  runTestTT createRightFacesTest
  runTestTT createLeftFacesTest
  runTestTT createLeftFacesMultiColumnTest
  runTestTT createHorizontallyAlignedCubesTest

singleDegree0RadiiTestData = SingleDegreeRadii 0 (map (Radius) [1,1,1])
singleDegree90RadiiTestData = SingleDegreeRadii 90 (map (Radius) [1,1,1])
singleDegree180RadiiTestData = SingleDegreeRadii 180 (map (Radius) [1,1,1])

createRightFacesTest = TestCase $ assertEqual
  "createRightFacesTest"
  ([RightFace {b3 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 10.0},
               b4 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 9.0},
               f3 = Point {x_axis = 0.0, y_axis = -1.0, z_axis = 10.0},
               f4 = Point {x_axis = 0.0, y_axis = -1.0, z_axis = 9.0}},
    RightFace {b3 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 9.0},
               b4 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 8.0},
               f3 = Point {x_axis = 0.0, y_axis = -1.0, z_axis = 9.0},
               f4 = Point {x_axis = 0.0, y_axis = -1.0, z_axis = 8.0}}] )
  (createRightFaces (Point 0 0 10) singleDegree0RadiiTestData flatXSlope flatYSlope [1..])


createLeftFacesTest = TestCase $ assertEqual
  "createLeftFacesTest"
  ([LeftFace  {b2 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 10.0},
               b1 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 9.0},
               f2 = Point {x_axis = 0.0, y_axis = -1.0, z_axis = 10.0},
               f1 = Point {x_axis = 0.0, y_axis = -1.0, z_axis = 9.0}},
    LeftFace  {b2 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 9.0},
               b1 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 8.0},
               f2 = Point {x_axis = 0.0, y_axis = -1.0, z_axis = 9.0},
               f1 = Point {x_axis = 0.0, y_axis = -1.0, z_axis = 8.0}}] )
  (createLeftFaces (Point 0 0 10) singleDegree0RadiiTestData flatXSlope flatYSlope [1..])

createLeftFacesMultiColumnTest = TestCase $ assertEqual
  "createLeftFacesMultiColumnTest"
  (
    [[
      LeftFace {b1 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 9.0}, 
                b2 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 10.0}, 
                f1 = Point {x_axis = 1.0, y_axis = 0.0, z_axis = 9.0}, 
                f2 = Point {x_axis = 1.0, y_axis = 0.0, z_axis = 10.0}},
      
      LeftFace {b1 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 8.0}, 
                b2 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 9.0}, 
                f1 = Point {x_axis = 1.0, y_axis = 0.0, z_axis = 8.0}, 
                f2 = Point {x_axis = 1.0, y_axis = 0.0, z_axis = 9.0}}],
      
     [LeftFace {b1 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 9.0}, 
                b2 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 10.0}, 
                f1 = Point {x_axis = 0.0, y_axis = 1.0, z_axis = 9.0}, 
                f2 = Point {x_axis = 0.0, y_axis = 1.0, z_axis = 10.0}},
      
      LeftFace {b1 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 8.0}, 
                b2 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 9.0}, 
                f1 = Point {x_axis = 0.0, y_axis = 1.0, z_axis = 8.0}, 
                f2 = Point {x_axis = 0.0, y_axis = 1.0, z_axis = 9.0}}]]
  )
  (
    createLeftFacesMultiColumns
      (Point 0 0 10)
      [singleDegree90RadiiTestData, singleDegree180RadiiTestData]
      flatXSlope flatYSlope
      [1..]
  )
createHorizontallyAlignedCubesTest = TestCase $ assertEqual
  "createHorizontallyAlignedCubesTest"
  (
    [
     [
      CubePoints {f1 = Point {x_axis = 1.0, y_axis = 0.0, z_axis = 9.0}, 
                 f2 = Point {x_axis = 1.0, y_axis = 0.0, z_axis = 10.0}, 
                 f3 = Point {x_axis = 0.0, y_axis = -1.0, z_axis = 10.0}, 
                 f4 = Point {x_axis = 0.0, y_axis = -1.0, z_axis = 9.0}, 
                 b1 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 9.0}, 
                 b2 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 10.0}, 
                 b3 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 10.0}, 
                 b4 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 9.0}},
     
     CubePoints {f1 = Point {x_axis = 0.0, y_axis = 1.0, z_axis = 9.0}, 
                 f2 = Point {x_axis = 0.0, y_axis = 1.0, z_axis = 10.0}, 
                 f3 = Point {x_axis = 1.0, y_axis = 0.0, z_axis = 10.0}, 
                 f4 = Point {x_axis = 1.0, y_axis = 0.0, z_axis = 9.0}, 
                 b1 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 9.0}, 
                 b2 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 10.0}, 
                 b3 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 10.0}, 
                 b4 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 9.0}}
     ],
     [
     CubePoints {f1 = Point {x_axis = 1.0, y_axis = 0.0, z_axis = 8.0}, 
                 f2 = Point {x_axis = 1.0, y_axis = 0.0, z_axis = 9.0}, 
                 f3 = Point {x_axis = 0.0, y_axis = -1.0, z_axis = 9.0}, 
                 f4 = Point {x_axis = 0.0, y_axis = -1.0, z_axis = 8.0}, 
                 b1 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 8.0}, 
                 b2 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 9.0}, 
                 b3 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 9.0}, 
                 b4 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 8.0}},
           
     CubePoints {f1 = Point {x_axis = 0.0, y_axis = 1.0, z_axis = 8.0}, 
                 f2 = Point {x_axis = 0.0, y_axis = 1.0, z_axis = 9.0}, 
                 f3 = Point {x_axis = 1.0, y_axis = 0.0, z_axis = 9.0}, 
                 f4 = Point {x_axis = 1.0, y_axis = 0.0, z_axis = 8.0}, 
                 b1 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 8.0}, 
                 b2 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 9.0}, 
                 b3 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 9.0}, 
                 b4 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 8.0}}
     ]
    ]
 
  )
  (
    createHorizontallyAlignedCubes
      (createRightFaces (Point 0 0 10) singleDegree0RadiiTestData flatXSlope flatYSlope [1..])
      (createLeftFacesMultiColumns
        (Point 0 0 10)
        [singleDegree90RadiiTestData, singleDegree180RadiiTestData]
        flatXSlope flatYSlope
        [1..]
      )
  )

