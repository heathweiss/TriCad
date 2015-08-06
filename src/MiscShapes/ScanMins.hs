{-# LANGUAGE ParallelListComp #-}
module MiscShapes.ScanTest1 () where
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.List.Split as LS
import TriCad.MathPolar(
  createRightFaces,
  createLeftFaces,
  Slope(..),
  Radius(..),
  flatXSlope,
  flatYSlope,
  createLeftFacesMultiColumns,
  createVerticalCubes,
  )
import TriCad.Points(Point(..))
import TriCad.CornerPoints(CornerPoints(..), (++>), (+++), (++++), Faces(..))
import TriCad.StlCornerPoints((+++^))
import TriCad.StlBase (StlShape(..), newStlShape)
import TriCad.StlFileWriter(writeStlToFile)
import Scan.Parse(parseToRadius)

generate :: IO ()
generate = do
  --file exists in Tricad folder, just about src
  contents <- BL.readFile "/home/heath/zeromq/openCvCsvFileWrite/mins.txt"

  let
      allDegreesRadius = parseToRadius contents
      heightPerPixel = 1
      origin = (Point{x_axis=0, y_axis=0, z_axis=50})

      imageCubes = generateCubesFromImage origin [0,90,180,270,360] heightPerPixel allDegreesRadius
      testCubes = generateSingleCube
      
      faces = --[FacesBottomFront | x <- [1..]]
         concat
          [[FacesFrontTop | x <- [1,2,3,4]],
           [FaceFront | x <- [5..472]],
           [FaceFront | x <- [473,474,475,476] ]
          ]
       
      triangles =
         faces +++^ imageCubes
      
      stlFile = newStlShape "joiner cube" triangles
  writeStlToFile stlFile
  --print $ show $ allCubes
  putStrLn "done"
  






{-
Generatethe cubes for a single degree.
singleDegreeCubes = pushSingleDegreeRightFacesOntoLeftFaces origin 0 heightPerPixel  (head allDegreesRadius )
-}
generateCubesFromImage :: Point -> [Double] -> Double -> [[Radius]] -> [CornerPoints]
generateCubesFromImage topOrigin degree heightPerPixel radii =
   let rightFace = createRightFaces topOrigin (head degree) flatXSlope flatYSlope [0,heightPerPixel..] (head radii)
       leftFaces = createLeftFacesMultiColumns topOrigin (tail degree) flatXSlope flatYSlope [0,heightPerPixel..] (tail radii)
            
       
   in createVerticalCubes rightFace leftFaces

generateSingleCube :: [CornerPoints]
generateSingleCube =

   let radii  = (parseToRadius $ BL.pack "1 2 3 4 5 6 7;1 2 3 4 5 6 7;1 2 3 4 5 6 7;1 2 3 4 5 6 7;1 2 3 4 5 6 7")
       origin = (Point{x_axis=0, y_axis=0, z_axis=50})
       heightPerPixel = 10
       degree = [0,90,180,270,360]
       leftFaces = createLeftFacesMultiColumns origin (tail degree) flatXSlope flatYSlope [0,heightPerPixel..] (tail radii)
       rightFaces =  createRightFaces origin (head degree) flatXSlope flatYSlope [0,heightPerPixel..] (head radii)
   in  createVerticalCubes rightFaces leftFaces

