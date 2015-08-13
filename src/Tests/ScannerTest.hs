module Tests.ScannerTest where
import Test.HUnit

import qualified Data.ByteString.Lazy.Char8 as BL
import TriCad.CornerPoints(CornerPoints(..), (++>), (+++), (++++), Faces(..))
import TriCad.Points(Point(..))
import TriCad.MathPolar(createRightFaces, createLeftFaces, createRightFacesFromScan, createLeftFacesFromScan,
                        Radius(..),Degree(..), Scan(..), flatXSlope, flatYSlope,
                        createVerticalCubes, createLeftFacesMultiColumns,
                        createVerticalCubesFromScan, createLeftFacesMultiColumnsFromScan,
                        createCornerPoint)
import Scan.Parse(parseToScan)
import Scan.Transform(minValueIndices, average)

scannerTestDo = do
  runTestTT createListOfCubesFromScanTest
  

createListOfCubesFromScanTest = TestCase $  assertEqual
  "create cubes from Scan"
  ([
   (CubePoints {b1=(Point  0 0 49), b2=(Point 0 0 50), b3=(Point 0 0 50 ), b4=(Point 0 0 49),
                f1=(Point 0.5 0 49), f2=(Point 0.5 0 50), f3=(Point 0 (-0.5) 50), f4=(Point 0 (-0.5) 49)}),

   (CubePoints {b1=(Point  0 0 49), b2=(Point 0 0 50), b3=(Point 0 0 50 ), b4=(Point 0 0 49),
                f1=(Point 0 0.5 49), f2=(Point 0 0.5 50), f3=(Point 0.5 0 50), f4=(Point 0.5 0 49)})
  ])
  (let scan  = (parseToScan (average . minValueIndices 2 ) "0 1 2 3;0 1 2 36$90 1 2 3;90 1 2 3$180 1 2 3;180 1 2 3")
       origin = (Point{x_axis=0, y_axis=0, z_axis=50})
       heightPerPixel = 1
       leftFaces = createLeftFacesMultiColumnsFromScan origin (tail $ degrees scan) flatXSlope flatYSlope [0,heightPerPixel..]
       rightFaces =  createRightFacesFromScan origin (head $ degrees scan) flatXSlope flatYSlope [0,heightPerPixel..]
   in  createVerticalCubesFromScan rightFaces leftFaces
  )


