module Tests.ScannerTest where
import Test.HUnit
import  Scan.Parse(parseToChar, parseToDouble, parseToRadius)
import qualified Data.ByteString.Lazy.Char8 as BL
import TriCad.CornerPoints(CornerPoints(..), (++>), (+++), (++++), Faces(..))
import TriCad.Points(Point(..))
import TriCad.MathPolar(createRightFaces, createLeftFaces, Radius(..), flatXSlope, flatYSlope,
                        createVerticalCubes, createLeftFacesMultiColumns)

scannerTestDo = do

  -- parsing raw data
  runTestTT parseToCharTest
  runTestTT parseToDoubleTest
  runTestTT parseToRadiusTest
  runTestTT parseToDoubleSingleColumnTest

  --creating faces
  runTestTT createListOfRightFacesTest
  runTestTT createListOfLeftFaces

  --creating cubes
  ------------------------------------- both these tests have to wait till I fix the trig
  --runTestTT createListOfCubes
  --runTestTT  createLeftFacesMultiColumnsTest
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
       (LeftFace {b2=(Point 0 0 50 ), b1=(Point 0 0 49), f2=(Point 0 (-1) 50 ), f1=(Point 0 (-2) 49)}),
       (LeftFace {b2=(Point 0 0 49 ), b1=(Point 0 0 48), f2=(Point 0 (-2) 49 ), f1=(Point 0 (-3) 48)})
    ])

   (
     let radii  = (parseToRadius $ BL.pack "1 2 3")
         origin = (Point{x_axis=0, y_axis=0, z_axis=50})
         heightPerPixel = 10
         degree = 0
     in
         createLeftFaces origin degree flatXSlope flatYSlope [0,1..] (head radii)
   )
{- Need to fix my trig caluclations before I can do this, as they are NFG!!!!!
createLeftFacesMultiColumnsTest = TestCase $ assertEqual
  "createLeftFacesMultiColumns test"
   ([
       [(LeftFace {b2=(Point 0 0 50 ), b1=(Point 0 0 49), f2=(Point 0 (-1) 50 ), f1=(Point 0 (-2) 49)})],
       [(LeftFace {b2=(Point 0 0 49 ), b1=(Point 0 0 48), f2=(Point 0 (-3) 49 ), f1=(Point 0 (-4) 48)})],
       [(LeftFace {b2=(Point 0 0 48 ), b1=(Point 0 0 47), f2=(Point 0 (-5) 48 ), f1=(Point 0 (-6) 47)})]
    ])
  (let radii = parseToRadius $ BL.pack "1 2;3 4;5 6"
       origin = (Point{x_axis=0, y_axis=0, z_axis=50})
       heightPerPixel = 1
       degrees = [0,90,180]
   in  createLeftFacesMultiColumns origin degrees flatXSlope flatYSlope  [0,heightPerPixel..] radii
     --createLeftFacesMultiColumns  topOrigin degrees     xSlope   ySlope zTransposeFactor radii
  )
-}
{- need to create leftFaces from [[Radius]] before I can do this
------------------------------------ creating cubes -----------------------------------
createListOfCubes = TestCase $  assertEqual
  "create cubes"
   ([
       (CubePoints {b1=(Point 0 0 50 ), b2=(Point 0 0 49), b3=(Point 0 (-1) 50 ), b4=(Point 0 (-2) 49),
        f1=(Point 0 0 49 ), f2=(Point 0 0 48), f3=(Point 0 (-2) 49 ), f4=(Point 0 (-3) 48)})
    ])
  (let radii  = (parseToRadius $ BL.pack "1 2 3;4 5 6;7 8 9")
       origin = (Point{x_axis=0, y_axis=0, z_axis=50})
       heightPerPixel = 10
       degree = [0,90,180]
       leftFaces = createLeftFaces origin degree flatXSlope flatYSlope [0,1..] (tail radii)
       rightFaces =  createRightFaces origin degree flatXSlope flatYSlope [0,1..] (head radii)
   in  createVerticalCubes rightFaces leftFaces
  )
-}
