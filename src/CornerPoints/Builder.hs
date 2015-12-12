module CornerPoints.Builder(CornerPointsBuilder(..),(&+++#@), (@~+++^), (|@~?+++^|), FacesWithRange(..), (&@~+++@),(&@~+++#@), (|||@~+++^|||),
                           processCornerPointsWithDegreesAndStl, newCornerPointsWith10DegreesBuilder) where
import CornerPoints.CornerPoints(CornerPoints(..), Faces(..))
import CornerPoints.CornerPointsWithDegrees(DegreeRange(..), CornerPointsWithDegrees(..), cubeIsWithinDegreeRange, (@~+++#@), (|@~+++#@|), (|@~+++@|),
                                           newCornerPointsWith10DegreesList)
import Stl.StlBase(Triangle(..))
import Stl.StlCornerPoints((+++^))
import CornerPoints.Degree(Degree(..))

-- ======================================= CornerPointsBuilder ===============================
{-Should be able to delete later on, to be replaced by CornerPointsWithDegrees.
Will need to change walkerSocket(Squared) first.

Should have just used a [[CornerPoints]] instead of new data type to hold them.
(&+++#@) could then have been #@:||@|| which is to say:
  Function mapped on the head [Cornerpoint], and appended to the [[CornerPoints]]-}


-- |=================== Depracated. Use CornerPointsWithDegrees in this module. ==============
--Building up a shape usually involves [[CornerPoints]]. This allow use of infix operators
--  to build up the shape in an monadic way, along with the use of &+++#@.
data CornerPointsBuilder  = CornerPointsBuilder {getCornerPoints :: [[CornerPoints]]}
  deriving (Eq, Show)
-- |=================== Depracated. Use CornerPointsWithDegrees in this module. ==============
--The infix operator to go along with CornerPointsBuilder for building up shapes as [[CornerPoints]]
(&+++#@) :: CornerPointsBuilder -> ([CornerPoints] -> [CornerPoints]) -> CornerPointsBuilder
(CornerPointsBuilder cornerPoints) &+++#@ f = CornerPointsBuilder ( (f $ head cornerPoints) : cornerPoints)


-- ======================================= CornerPointsWithDegrees ================================
data FacesWithRange = FacesWithRange {_face::Faces, _range::DegreeRange}




(@~+++^) :: Faces -> CornerPointsWithDegrees -> [Triangle]
face @~+++^ (CubesWithStartEndDegrees cPnt _) = face +++^ cPnt

(|@~?+++^|) :: FacesWithRange -> [CornerPointsWithDegrees] -> [[Triangle]] 
(FacesWithRange face range) |@~?+++^| cornerPointsWithDegrees   =
     [face @~+++^  x | x <-  cubeIsWithinDegreeRange range  cornerPointsWithDegrees]

(||@~?+++^||) :: [FacesWithRange] -> [CornerPointsWithDegrees] -> [[Triangle]]
facesWithRanges ||@~?+++^|| cornerPointsWithDegrees =
  concat  [x |@~?+++^| cornerPointsWithDegrees | x <- facesWithRanges]

--add a CornerPoint to the head of the [[CornerPointsWithDegrees]] in list fashion
(&@~+++@) :: [[CornerPointsWithDegrees]] -> [CornerPoints] -> [[CornerPointsWithDegrees]]
cornerPointsWithDegreesListList &@~+++@ cornerPointsList =
   ((head cornerPointsWithDegreesListList )  |@~+++@| cornerPointsList) : cornerPointsWithDegreesListList

(&@~+++#@) :: [[CornerPointsWithDegrees]] -> (CornerPoints -> CornerPoints) -> [[CornerPointsWithDegrees]]
cornerPointsWithDegreesListList &@~+++#@ f =
  ((head cornerPointsWithDegreesListList ) |@~+++#@| f)  : cornerPointsWithDegreesListList

{-A builder which uses a CornerPointsWithDegrees-}
--     ------------------------------------------------------- probably delete everything below here -----------------------------------
data CornerPointsWithDegreesAndStl =
  CornerPointsWithDegreesAndStl {_cornerPoints:: [CornerPointsWithDegrees],
                                 _facesWithRange:: [FacesWithRange] }

{- |Process a [CornerPointsWithDegrees] into stl [Triangle]'s. Usally used via (|||@~+++^|||) to process an entire shape. -}
--ToDo: See about not exporting this. Perhaps move into a let statement of (|||@~+++^|||)
processCornerPointsWithDegreesAndStl ::  [CornerPointsWithDegrees] -> [FacesWithRange] -> [Triangle]
processCornerPointsWithDegreesAndStl cornerPointsList facesWithRangeList =
  concat $ facesWithRangeList  ||@~?+++^|| cornerPointsList

{- | Process a shape made up of [[CornerPointsWithDegrees]] into stl [Triangle]'s for output to stl file. -}
(|||@~+++^|||) :: [[CornerPointsWithDegrees]] -> [[FacesWithRange]] -> [Triangle]
cornerPointsWithDegreesList |||@~+++^||| facesWithRangeList = concat $
  zipWith processCornerPointsWithDegreesAndStl cornerPointsWithDegreesList facesWithRangeList

newCornerPointsWith10DegreesBuilder :: [CornerPoints] -> [[CornerPointsWithDegrees]]
newCornerPointsWith10DegreesBuilder    cornerPoints   = [newCornerPointsWith10DegreesList cornerPoints]
