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


generate :: IO ()
generate = do
  --file exists in Tricad folder, just about src
  contents <- BL.readFile "scanTest1.txt"

  let
      --the length of splitColon is the number of scans done.
      splitColon =  LS.splitOn ";"   $ BL.unpack contents
      splitSpace =  map (LS.splitOn " ") splitColon
      --array length = 5 as that is how many were read from the file
      arrayOfRadiusForEachDegree =  [ map (Radius .readDouble) x |  x <- splitSpace]
      heightPerPixel = 0.1
      origin = (Point{x_axis=0, y_axis=0, z_axis=50})
      
      
              
  print $ show $ generateLeftFaces origin (head arrayOfRadiusForEachDegree) heightPerPixel




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
Will be different from subsequent lines because this on needs to be a front right line
while all the others will be a front left line.

-}
generateRightFaces :: Point ->  [Radius] -> Double -> [CornerPoints]
generateRightFaces topOrigin radList heightPerPixel  = 
  --first point will be a front right top
  --all the rest will be bottom front right
  createRightFaces topOrigin 0 flatXSlope flatYSlope [0.0,heightPerPixel..] radList

{-
For now it just creates a [LeftFace] from the 0 degree Radius
Next figure out how to map this onto the tail of all the degrees Radius.
Then figure out how to [RighFace] ++> [[LeftFace] as opposed to what I have done before. eg:
 RightFace ++> [LeftFace]
-}
generateLeftFaces :: Point ->  [Radius] -> Double -> [CornerPoints]
generateLeftFaces topOrigin radList heightPerPixel  = 
  --first point will be a front right top
  --all the rest will be bottom front right
  createLeftFaces topOrigin 0 flatXSlope flatYSlope [0.0,heightPerPixel..] radList
