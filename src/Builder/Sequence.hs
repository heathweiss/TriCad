{-# LANGUAGE ViewPatterns #-}
module Builder.Sequence(newCornerPointsWith10DegreesBuilder, newCornerPointsWith5DegreesBuilder, (||@~+++^||), (@~+++#@|>), (@~+++@|>)) where
import Builder.Builder(FacesWithRange(..), (||@~?+++^||), processCornerPointsWithDegreesAndStl)
import CornerPoints.CornerPointsWithDegrees(DegreeRange(..), CornerPointsWithDegrees(..), cubeIsWithinDegreeRange, (@~+++#@), (|@~+++#@|), (|@~+++@|),
                                           newCornerPointsWith10DegreesList, newCornerPointsWith5DegreesList)
import CornerPoints.CornerPoints(CornerPoints(..))
import Stl.StlBase(Triangle(..))
import Stl.StlCornerPoints((+++^), Faces(..))
import qualified Data.Sequence as S
import qualified Data.Foldable as F

data CornerPointsWithDegreesList = CornerPointsWithDegreesList {cornerPointsWithDegreesList::[CornerPointsWithDegrees]}

--get the last [CornerPointsWithDegrees] in the (Seq [CornerPointsWithDegrees])
x :: (S.Seq a) -> a
x (S.viewr -> xs S.:> x') = x'

{- |

Apply (|@~+++@| cornerPoint ) to the right end of the sequence
and add result to the right end of the sequence.
-}
(@~+++@|>) :: (S.Seq [CornerPointsWithDegrees]) -> [CornerPoints] -> (S.Seq [CornerPointsWithDegrees])
cornerPointsSeq @~+++@|> cornerPointsList =
  cornerPointsSeq  S.|>  ((x cornerPointsSeq) |@~+++@| cornerPointsList)


{- |
Apply (|@~+++#@| cornerPoint ) to the right end of the sequence
and add result to the right end of the sequence.
-}
(@~+++#@|>) :: (S.Seq [CornerPointsWithDegrees]) -> (CornerPoints -> CornerPoints) -> (S.Seq [CornerPointsWithDegrees])
cornerPointsSeq @~+++#@|> f =
  cornerPointsSeq  S.|>  ((x cornerPointsSeq) |@~+++#@| f)
  
{- |
Process a shape made up of Seq[CornerPointsWithDegrees] into stl [Triangle]'s for output to stl file. 
-}
(||@~+++^||) :: (S.Seq[CornerPointsWithDegrees]) -> [[FacesWithRange]] -> [Triangle]
cornerPointsWithDegreesSeq ||@~+++^|| facesWithRangeList = concat $
  zipWith processCornerPointsWithDegreesAndStl (F.toList cornerPointsWithDegreesSeq) facesWithRangeList


{- |
Used by numerous infix functions such as (&@~+++#@) for building up a [[CornerPointsWithDegrees]].
Each layer of a stl shape is made up of [CornerPointsWithDegrees].
This Builder allows these layer to be built up, by adding another top/bottome face to the top of the
Builder list.

The 10 indicates it is based on a 10 degree interval of the radial shape.
Eg: A scan that is taken at 10 degree intervals such as 0,10..360
-}
newCornerPointsWith10DegreesBuilder :: [CornerPoints] -> (S.Seq [CornerPointsWithDegrees])
newCornerPointsWith10DegreesBuilder    cornerPoints   = S.singleton $ newCornerPointsWith10DegreesList cornerPoints

{- |
Used by numerous infix functions such as (&@~+++#@) for building up a [[CornerPointsWithDegrees]].
Each layer of a stl shape is made up of [CornerPointsWithDegrees].
This Builder allows these layer to be built up, by adding another top/bottome face to the top of the
Builder list.

The 5 indicates it is based on a 5 degree interval of the radial shape.
Eg: A scan that is taken at 5 degree intervals such as 0,5..360
-}
newCornerPointsWith5DegreesBuilder :: [CornerPoints] -> (S.Seq [CornerPointsWithDegrees])
newCornerPointsWith5DegreesBuilder    cornerPoints   = S.singleton $ newCornerPointsWith5DegreesList cornerPoints
