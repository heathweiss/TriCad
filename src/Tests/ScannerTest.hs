module Tests.ScannerTest where
import Test.HUnit
import  Scan.Parse.Mins(parseToChar, parseToDouble, parseToRadius)
import qualified Data.ByteString.Lazy.Char8 as BL
import TriCad.CornerPoints(CornerPoints(..), (++>), (+++), (++++), Faces(..))
import TriCad.Points(Point(..))
import TriCad.MathPolar(createRightFaces, createLeftFaces, Radius(..), flatXSlope, flatYSlope,
                        createVerticalCubes, createLeftFacesMultiColumns, createCornerPoint)

scannerTestDo = do

  -- parsing raw data
  runTestTT parseToCharTest
  runTestTT parseToDoubleTest
  runTestTT parseToRadiusTest
  runTestTT parseToDoubleSingleColumnTest

  --creating faces
  runTestTT createListOfRightFacesTest
  runTestTT createListOfLeftFaces
  runTestTT createRightFacesAt90DegreesTest

  --creating cubes
  ------------------------------------- both these tests have to wait till I fix the trig
  runTestTT createListOfCubes
  runTestTT  createLeftFacesMultiColumnsTest
--------------------------------------------- parsing the raw data -------------------------------
parseToCharTest = TestCase $ assertEqual 
  "parseToChareee"
  ([["1","2"],["3","4"]])
  (parseToChar $ BL.pack "1 2;3 4")

parseToDoubleTest = TestCase $ assertEqual
  "parseToDoubleTest"
  ([[1,2],[3,4]])
  (parseToDouble $ BL.pack "1 2;3 4")

parseToDoubleSingleColumnTest = TestCase $ assertEqual
  "parseToDoubleTest with a single column"
  ([[1,2]])
  (parseToDouble $ BL.pack "1 2")

parseToRadiusTest = TestCase $ assertEqual
  "parseToRadiusTest"
  ([[(Radius 1),(Radius 2)],[(Radius 3),(Radius 4)]])
  (parseToRadius $ BL.pack "1 2;3 4")

  

-------------------------------------------- creating faces -----------------------------------



createRightFacesAt90DegreesTest = TestCase $ assertEqual
   "create right faces 90 degrees"
   ([
       (RightFace {b3=(Point 0 0 50 ), b4=(Point 0 0 49), f3=(Point 1 0 50 ), f4=(Point 2 0 49)}),
       (RightFace {b3=(Point 0 0 49 ), b4=(Point 0 0 48), f3=(Point 2 0 49 ), f4=(Point 3 0 48)})
    ])

   (
     let radii  = (parseToRadius $ BL.pack "1 2 3")
         origin = (Point{x_axis=0, y_axis=0, z_axis=50})
         heightPerPixel = 10
         degree = 90
     in
         createRightFaces origin degree flatXSlope flatYSlope [0,1..] (head radii)
   )

createListOfRightFacesTest = TestCase $  assertEqual 
   "create right faces"
   ([
       (RightFace {b3=(Point 0 0 50 ), b4=(Point 0 0 49), f3=(Point 0 (-1) 50 ), f4=(Point 0 (-2) 49)}),
       (RightFace {b3=(Point 0 0 49 ), b4=(Point 0 0 48), f3=(Point 0 (-2) 49 ), f4=(Point 0 (-3) 48)})
    ])

   (
     let radii  = (parseToRadius $ BL.pack "1 2 3")
         origin = (Point{x_axis=0, y_axis=0, z_axis=50})
         heightPerPixel = 10
         degree = 0
     in
         createRightFaces origin degree flatXSlope flatYSlope [0,1..] (head radii)
   )

createListOfLeftFaces  = TestCase $  assertEqual 
   "create left faces"
   ([
       (LeftFace {b1=(Point 0 0 49), b2=(Point 0 0 50 ), f1=(Point 0 (-2) 49), f2=(Point 0 (-1) 50 )}),
       (LeftFace {b1=(Point 0 0 48), b2=(Point 0 0 49 ), f1=(Point 0 (-3) 48), f2=(Point 0 (-2) 49 )})
    ])

   (
     let radii  = (parseToRadius $ BL.pack "1 2 3")
         origin = (Point{x_axis=0, y_axis=0, z_axis=50})
         heightPerPixel = 10
         degree = 0
     in
         createLeftFaces origin degree flatXSlope flatYSlope [0,1..] (head radii)
   )
{-
Create 3 columns, each with just 1 face, which requires 2 lines per column.
-}
createLeftFacesMultiColumnsTest = TestCase $ assertEqual
  "createLeftFacesMultiColumns test"
   ([
       [(LeftFace {b1=(Point 0 0 49), b2=(Point 0 0 50 ), f1=(Point 0 (-2) 49),  f2=(Point 0 (-1) 50 )})],
       [(LeftFace {b1=(Point 0 0 49), b2=(Point 0 0 50 ), f1=(Point 4 0 49) ,    f2=(Point 3 0 50 )})],
       [(LeftFace {b1=(Point 0 0 49), b2=(Point 0 0 50 ), f1=(Point 0 6 49),     f2=(Point 0 5 50 )})]
    ])
  (let radii = parseToRadius $ BL.pack "1 2;3 4;5 6"
       origin = (Point{x_axis=0, y_axis=0, z_axis=50})
       heightPerPixel = 1
       degrees = [0,90,180]
   in  createLeftFacesMultiColumns origin degrees flatXSlope flatYSlope  [0,heightPerPixel..] radii
     
  )


------------------------------------ creating cubes -----------------------------------
createListOfCubes = TestCase $  assertEqual
  "create cubes"
  ([
   (CubePoints {b1=(Point  0 0 49), b2=(Point 0 0 50), b3=(Point 0 0 50 ), b4=(Point 0 0 49),
                f1=(Point 4 0 49), f2=(Point 3 0 50), f3=(Point 0 (-1) 50), f4=(Point 0 (-2) 49)}),
   (CubePoints {b1=(Point 0 0 49), b2=(Point 0 0 50), b3=(Point 0 0 50), b4=(Point 0 0 49),
                f1=(Point 0 6 49), f2=(Point 0 5 50), f3=(Point 3 0 50), f4=(Point 4 0 49)})
  ])
  (let radii  = (parseToRadius $ BL.pack "1 2;3 4;5 6")
       origin = (Point{x_axis=0, y_axis=0, z_axis=50})
       heightPerPixel = 1
       degree = [0,90,180]
       leftFaces = createLeftFacesMultiColumns origin (tail degree) flatXSlope flatYSlope [0,heightPerPixel..] (tail radii)
       rightFaces =  createRightFaces origin (head degree) flatXSlope flatYSlope [0,heightPerPixel..] (head radii)
   in  createVerticalCubes rightFaces leftFaces
  )

----------------------------------------------------------------------------------------------------------------------------------------------------------------------------- repeat all tests for the new Scan datatype-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
