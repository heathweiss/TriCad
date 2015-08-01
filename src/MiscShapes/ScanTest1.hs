module ScanTest1 where
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.List.Split as LS
import TriCad.MathPolar(
  createTopFaces,
  createBottomFaces,
  createCornerPoint,
  Slope(..),
  Radius(..),
  )

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

      
      
              
  print $ show $ createFirstVerticalLine $ head arrayOfRadiusForEachDegree




{-
Mapped over the string file, to get the ints.
Could probably use something like read::Int.
Or used the Numeric package
-}
readInt :: String -> Int
readInt str = read str

readDouble :: String -> Double
readDouble str = read str


createFirstVerticalLine :: [Radius] -> Int
createFirstVerticalLine radList = length radList
