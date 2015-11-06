module CornerPoints.Radius(Radius(..), SingleDegreeRadii(..), Degree(..), MultiDegreeRadii(..),
                          extractSingle, extractList) where
import CornerPoints.Transposable( TransposeLength, transpose)

{-|
Represents a radius of a circular shape, which is what all shapes in math polar are created from.
-}
data Radius = Radius {radius :: Double}
   deriving (Show)

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


class ExtractableRadius a  where
  extractList :: ([Radius] -> [Radius]) -> a -> a
  extractSingle :: ([Radius] -> Radius) -> a -> a

instance ExtractableRadius SingleDegreeRadii where
  extractSingle f (SingleDegreeRadii degree' radii') = SingleDegreeRadii degree' [(f radii')]
  extractList   f (SingleDegreeRadii degree' radii') = SingleDegreeRadii degree' (f radii')
  
instance ExtractableRadius MultiDegreeRadii where
  extractSingle f (MultiDegreeRadii name' degrees') = MultiDegreeRadii name' (map (extractSingle f) degrees')
  extractList f (MultiDegreeRadii name' degrees') = MultiDegreeRadii name' (map (extractList f) degrees')
