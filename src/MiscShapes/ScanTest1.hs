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
  )
import TriCad.Points(Point(..))
import TriCad.CornerPoints(CornerPoints(..), (++>), (+++), (++++), Faces(..))
import TriCad.StlCornerPoints((+++^))
import TriCad.StlBase (StlShape(..), newStlShape)
import TriCad.StlFileWriter(writeStlToFile)

generate :: IO ()
generate = do
  --file exists in Tricad folder, just about src
  contents <- BL.readFile "scanTest1.txt"

  let
      --the length of splitColon is the number of scans done.
      splitColon =  LS.splitOn ";"   $ BL.unpack contents
      splitSpace =  map (LS.splitOn " ") splitColon
      --array length = 5 as that is how many were read from the file
      allDegreesRadius =  [ map (Radius .readDouble) x |  x <- splitSpace]
      heightPerPixel = 10
      origin = (Point{x_axis=0, y_axis=0, z_axis=50})

      allCubes = generateCubes origin [0,90,180,270,360] heightPerPixel allDegreesRadius
      
      
      faces = [FacesAll | x <- [1..]]
         --concat
         -- [[FacesFrontTop],
         --  [FaceFront | x <- [2..477]],
         --  [FacesAll]
         -- ]
       
      triangles =
         faces +++^ allCubes
      
      stlFile = newStlShape "joiner cube" triangles
  writeStlToFile stlFile
  --print $ show $ allCubes
  putStrLn "done"
  



{-
Mapped over the string file, to get the ints.
Could probably use something like read::Int.
Or used the Numeric package
-}
readInt :: String -> Int
readInt str = read str

readDouble :: String -> Double
readDouble str = read str

{-
Will be different from subsequent lines because this one needs to be a front right line
while all the others will be a front left line.
It only needs a single degree passed in, probably 0, as it is only processing a single [Radius]
-}
generateRightFaces :: Point ->  Double -> Double -> [Radius] -> [CornerPoints]
generateRightFaces topOrigin heightPerPixel degree radList  = 
  --first point will be a front right top
  --all the rest will be bottom front right
  createRightFaces topOrigin degree flatXSlope flatYSlope [0,heightPerPixel..] radList

{-
Creates [[LeftFace]] from the [degrees] and [[Radius]]



generateLeftFaces :: Point -> Double ->  [Double] -> [[Radius]]  -> [[CornerPoints]]
generateLeftFaces topOrigin heightPerPixel degrees radList   = 
  --first point will be a front right top
  --all the rest will be bottom front right
  --createLeftFaces topOrigin 0 flatXSlope flatYSlope [0.0,heightPerPixel..] radList
  [ createLeftFaces topOrigin degree flatXSlope flatYSlope [0.0,heightPerPixel..]  radius
    | degree <- degrees
    | radius <- radList
  ]


createLeftFaces :: Point -> Double -> Slope -> Slope -> [Double] -> [Radius] -> [CornerPoints]
createLeftFaces topOrigin degree xSlope ySlope zTransposeFactor inRadius  =
  createVerticalFaces topOrigin degree xSlope ySlope zTransposeFactor inRadius (F2) (B2) (F1) (B1)
-}
generateLeftFaces :: Point -> Double ->  Double -> [Radius]  -> [CornerPoints]
generateLeftFaces topOrigin heightPerPixel degree radList   = 
  --first point will be a front right top
  --all the rest will be bottom front right
  --createLeftFaces topOrigin 0 flatXSlope flatYSlope [0.0,heightPerPixel..] radList
  createLeftFaces topOrigin degree flatXSlope flatYSlope [1,heightPerPixel..] radList

{-
Generatethe cubes for a single degree.
singleDegreeCubes = pushSingleDegreeRightFacesOntoLeftFaces origin 0 heightPerPixel  (head allDegreesRadius )
-}
generateCubes :: Point -> [Double] -> Double -> [[Radius]] -> [CornerPoints]
generateCubes topOrigin degree heightPerPixel radii =
   let rightFace = createRightFaces topOrigin (head degree) flatXSlope flatYSlope [1,heightPerPixel..] (head radii)
       leftFaces = --createLeftFaces topOrigin 90 flatXSlope flatYSlope [10,heightPerPixel..] (tail radii)
                   [ createLeftFaces topOrigin degreeLcl flatXSlope flatYSlope [1,heightPerPixel..] radiiLcl
                     | degreeLcl <- tail degree
                     | radiiLcl <- tail radii
                   ]
       allCubes =
        [rightFacelcl ++> leftFaceslcl
         | rightFacelcl <- rightFace
         | leftFaceslcl <- leftFaces
        ]
   in
     concat allCubes    
{-
Print the cubes for a single degree
-}
printSingleDegree :: [CornerPoints] -> String
printSingleDegree cubepoints =
   let faces =
          [FacesAll]
         --[[FacesFrontTop],
         --  [FaceFront | x <- [2..477]],
         --  [FacesAll]
         -- ]

       triangles =
         faces +++^ cubepoints

       stlFile = newStlShape "joiner cube" triangles
   in
    --writeStlToFile stlFile
   "hello"
