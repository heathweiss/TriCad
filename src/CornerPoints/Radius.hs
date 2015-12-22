
module CornerPoints.Radius(Radius(..), SingleDegreeRadii(..), Degree(..), MultiDegreeRadii(..),resetMultiDegreeRadiiIfNull,
                          extractSingle, extractList, rotateMDR, setRadiusIfNull, resetSingleDegreeRadiiIfNull,
                          setRadiusWithPrecedingValueIfNull, resetMultiDegreeRadiiIfNullWithPreviousValue,
                          buildSymmetricalRadius) where
import CornerPoints.Transposable( TransposeLength, transpose)
import Data.List(sortBy)
import Data.Ord (Ordering(..), comparing)
import CornerPoints.CornerPoints(CornerPoints(..))

{-|
Represents a radius of a circular shape, which is what all shapes in math polar are created from.
-}
data Radius = Radius {radius :: Double}
              
   deriving (Show)

-- | Reset value of a Radius if null.
setRadiusIfNull :: Double -> Radius -> Radius
setRadiusIfNull resetValue (Radius radius')
  | isNaN radius' = Radius resetValue 
  | otherwise = Radius radius'
  
-- | Reset all null value Radius with the preceding Radius.
--   Pass in a value to start off the list, in case 1st Radius is null.
setRadiusWithPrecedingValueIfNull :: Double -> [Radius] -> [Radius]
setRadiusWithPrecedingValueIfNull resetValue (x:xs) =
  let currRadius = setRadiusIfNull resetValue x
  in currRadius : setRadiusWithPrecedingValueIfNull (radius currRadius) xs
setRadiusWithPrecedingValueIfNull resetValue [] = []
  

-- |Check for equaility of Radius and RadiusNaN
radiusEqual :: (Eq a, Num a, Ord a, Fractional a) => a -> a -> Bool
radiusEqual  a b
  
  | (abs (a - b)) <= 0.011 = True
  | otherwise      = False

instance Eq Radius where
    Radius rad == Radius rad'
      | (radiusEqual rad rad') = True 
      | otherwise = False

instance TransposeLength Radius where
  transpose f (Radius a) = Radius $ f a

{- |Build the Radius for a full 360 degree shape that is symmetrical.
    The halfDegrees represents 0-170 degrees. centerDegree is the 180 degree.

Known uses: Creating the heel of a shoe, which is typically symmetrical.-}
buildSymmetricalRadius :: [Degree] -> Degree -> [Radius]
buildSymmetricalRadius    halfDegrees centerDegree =
   map Radius $  halfDegrees ++ ( centerDegree : (reverse halfDegrees))

-- | Reset all Radius Null to a Radius defaultValue
resetSingleDegreeRadiiIfNull :: Double ->  SingleDegreeRadii -> SingleDegreeRadii
resetSingleDegreeRadiiIfNull resetValue    (SingleDegreeRadii degree' radii') =
  SingleDegreeRadii degree' $ map (setRadiusIfNull resetValue) radii'



-- | Reset  Radius Null to previous  Radius value.
--   If it is the very first Radius, give it a default value.

resetSingleDegreeRadiiIfNullWithPreviousValue :: Double ->  SingleDegreeRadii -> SingleDegreeRadii
resetSingleDegreeRadiiIfNullWithPreviousValue resetValue    (SingleDegreeRadii degree' radii') =
  SingleDegreeRadii degree' $ setRadiusWithPrecedingValueIfNull resetValue radii'
 
{-
Contains the [Radius] associated with a single degree from a vertical scan.

Scan.Json module declares it an instance of ToJSON and FromJSON for the aeson package.

Known uses:
Raw image data is parsed into Scan datatype, which contains [Degree]. This is then
processed into cubes.

Store the processed raw data as json, so the processing only has to be done once.
-}
data SingleDegreeRadii = SingleDegreeRadii {degree::Degree, radii::[Radius]}
     deriving (Show, Eq)

instance TransposeLength SingleDegreeRadii  where
  transpose f (SingleDegreeRadii degree' radii') = SingleDegreeRadii degree' (map (transpose f) radii')



-- |Degree of a circle.
type Degree = Double


{- |
Contains all the filtered data from a scan.
Is a [SingleDegreeRadii] and an assoc'd name.

Known uses:
Raw scan image data is processed into this, which is the last ADT, before being turned in CornerPoints.
It can be read to/from json, so that all the processing of scan data, can be saved to file.
-}
data MultiDegreeRadii = MultiDegreeRadii {name::String, degrees::[SingleDegreeRadii]}
          deriving (Show, Eq)

instance TransposeLength MultiDegreeRadii  where
  transpose f (MultiDegreeRadii name' degrees') = MultiDegreeRadii name' (map (transpose f) degrees')

-- |Rotate the radii clockwise on the xy plane.
--  Shifts the [Radius] up to the next SingleDegreeRadii in the degrees field,
--  while preserving the fact that the first and last degree must always having matching [Radius].
rotateMDR ::  MultiDegreeRadii -> MultiDegreeRadii
rotateMDR     multiDegreeRadii   =
  let
     -- Rotate the radii clockwise on the xy plane.
     --  Shifts the [Radius] up to the next SingleDegreeRadii in the degrees field.
     -- The first and and last degree [Radius] must always match, so this inital call
     -- needs to set the new first degree [Radius] from the 2nd last entry, as that will become the new last entry.
     rotateSDR  :: [SingleDegreeRadii]  -> [SingleDegreeRadii]
     rotateSDR (x:xs)  =
       (x {radii = (radii $ last $ init xs)}) :  rotateSDRRecur (radii x) xs
     rotateSDRRecur :: [Radius] -> [SingleDegreeRadii] -> [SingleDegreeRadii]
     rotateSDRRecur radii'   (x:xs) =
      (x {radii = radii'}) : (rotateSDRRecur (radii  x) xs)
     rotateSDRRecur radii' [] = []
     
  in 
     multiDegreeRadii {degrees = (rotateSDR  (degrees multiDegreeRadii ))}

-- | Reset all Radii Null values with a default value
resetMultiDegreeRadiiIfNull :: Double -> MultiDegreeRadii -> MultiDegreeRadii
resetMultiDegreeRadiiIfNull resetValue (MultiDegreeRadii name' degrees') =
  MultiDegreeRadii name' $ map (resetSingleDegreeRadiiIfNull resetValue) degrees'

-- | Reset all Radii Null values with the previous Radius. Provide a starter Radius for start of list.
resetMultiDegreeRadiiIfNullWithPreviousValue :: Double -> MultiDegreeRadii -> MultiDegreeRadii
resetMultiDegreeRadiiIfNullWithPreviousValue resetValue (MultiDegreeRadii name' degrees') =
  MultiDegreeRadii name' $ map (resetSingleDegreeRadiiIfNullWithPreviousValue resetValue) degrees'

class ExtractableRadius a  where
  -- |Know instances:
  -- MultiDegreeRadii uses it to extract [horizontal row of SingleDegreeRadii]
  -- Ex: Pick the top n rows from a scan.
  extractList :: ([Radius] -> [Radius]) -> a -> a
  -- |Know instances:
  -- MultiDegreeRadii uses it to extract a single horizontal row of SingleDegreeRadii.
  -- Ex: Pick the top row from a scan.
  extractSingle :: ([Radius] -> Radius) -> a -> a
  

instance ExtractableRadius SingleDegreeRadii where
  extractSingle f (SingleDegreeRadii degree' radii') = SingleDegreeRadii degree' [(f radii')]
  extractList   f (SingleDegreeRadii degree' radii') = SingleDegreeRadii degree' (f radii')
  
instance ExtractableRadius MultiDegreeRadii where
  extractSingle f (MultiDegreeRadii name' degrees') = MultiDegreeRadii name' (map (extractSingle f) degrees')
  extractList f (MultiDegreeRadii name' degrees') = MultiDegreeRadii name' (map (extractList f) degrees')


type RotateFactor = Double

{- outline for a Radius list
    ,--10
    ,--20
    ,--30
    ,--40
    ,--50
    ,--60
    ,--70
    ,--80
    ,--90
    ,--100
    ,--110
    ,--120
    ,--130
    ,--140
    ,--150
    ,--160
    ,--170
    ,--180
    ,--190
    ,--200
    ,--210
    ,--220
    ,--230
    ,--240
    ,--250
    ,--260
    ,--270
    ,--280
    ,--290
    ,--300
    ,--310
    ,--320
    ,--330
    ,--340
    ,--350
    --360

  ]


   [ ,--0
    ,--5
    ,--10
    ,--15
    ,--20
    ,--25
    ,--30
    ,--35
    ,--40
    ,--45
    ,--50
    ,--55
    ,--60
    ,--65
    ,--70
    ,--75
    ,--80
    ,--85
    ,--90
    ,--95
    ,--100
    ,--105
    ,--110
    ,--115
    ,--120
    ,--125
    ,--130
    ,--135
    ,--140
    ,--145
    ,--150
    ,--155
    ,--160
    ,--165
    ,--170
    ,--175
    ,--180
    ,--185
    ,--190
    ,--195
    ,--200
    ,--205
    ,--210
    ,--215
    ,--220
    ,--225
    ,--230
    ,--235
    ,--240
    ,--245
    ,--250
    ,--255
    ,--260
    ,--265
    ,--270
    ,--275
    ,--280
    ,--285
    ,--290
    ,--295
    ,--300
    ,--305
    ,--310
    ,--315
    ,--320
    ,--325
    ,--330
    ,--335
    ,--340
    ,--345
    ,--350
    ,--355
    --360

  ]
-}
