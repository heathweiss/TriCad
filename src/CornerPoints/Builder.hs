module CornerPoints.Builder(CornerPointsBuilder(..),(&+++#@), (@~+++^), (|@~?|+++^), FacesWithRange(..), (&@~#@~), (&@~+++@),(&@~+++#@),
                           processCornerPointsWithDegreesAndStl) where
import CornerPoints.CornerPoints(CornerPoints(..), Faces(..))
import CornerPoints.CornerPointsWithDegrees(DegreeRange(..), CornerPointsWithDegrees(..), cubesWithinDegreeRange, (@~+++#@), (|@~+++#@|), (|@~+++@|))
import Stl.StlBase(Triangle(..))
import Stl.StlCornerPoints((+++^))


-- ======================================= CornerPointsBuilder ===============================
{-Should be abel to delete later on.

Should have just used a [[CornerPoints]] instead of new data type to hold them.
(&+++#@) could then have been #@:||@|| which is to say:
  Function mapped on the head [Cornerpoint], and appended to the [[CornerPoints]]-}


-- |Building up a shape usually involves [[CornerPoints]]. This allow use of infix operators
--  to build up the shape in an monadic way, along with the use of &+++#@.
data CornerPointsBuilder  = CornerPointsBuilder {getCornerPoints :: [[CornerPoints]]}
  deriving (Eq, Show)
-- |The infix operator to go along with CornerPointsBuilder for building up shapes as [[CornerPoints]]
(&+++#@) :: CornerPointsBuilder -> ([CornerPoints] -> [CornerPoints]) -> CornerPointsBuilder
(CornerPointsBuilder cornerPoints) &+++#@ f = CornerPointsBuilder ( (f $ head cornerPoints) : cornerPoints)


-- ======================================= CornerPointsWithDegrees ================================
data FacesWithRange = FacesWithRange {_face::Faces, _range::DegreeRange}




(@~+++^) :: Faces -> CornerPointsWithDegrees -> [Triangle]
face @~+++^ (CubesWithStartEndDegrees cPnt _) = face +++^ cPnt

{- |Filter a list of CornerPointsWithDegrees, for those within a DegreeRange, and output the stl Triangles for them.-}
(|@~?|+++^) :: FacesWithRange -> [CornerPointsWithDegrees] -> [[Triangle]] 
(FacesWithRange face range) |@~?|+++^ cornerPointsWithDegrees   =
     [face @~+++^  x | x <-  cubesWithinDegreeRange range  cornerPointsWithDegrees]


{-[CornerPointsWithDegrees] [FacesWithRange]
-}
(|@~?||~^|+++^) :: [FacesWithRange] -> [CornerPointsWithDegrees] -> [[Triangle]]
facesWithRanges |@~?||~^|+++^ cornerPointsWithDegrees =
  concat  [x |@~?|+++^ cornerPointsWithDegrees | x <- facesWithRanges]

--ToDo: Could make this builder take polymorphic types, so it would be general in applying (or not) something to top of list and appending to list
(&@~#@~) :: [[CornerPointsWithDegrees]] -> ([CornerPointsWithDegrees] -> [CornerPointsWithDegrees]) -> [[CornerPointsWithDegrees]]
cornerPointsWithDegreesListList &@~#@~ f =
  (f $ head cornerPointsWithDegreesListList) : cornerPointsWithDegreesListList

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

processCornerPointsWithDegreesAndStl ::  [CornerPointsWithDegrees] -> [FacesWithRange] -> [Triangle]
processCornerPointsWithDegreesAndStl cornerPointsList facesWithRangeList =
  concat $ facesWithRangeList  |@~?||~^|+++^ cornerPointsList




{-This builder failed because it incorporates STL info in it, which has to change each time.
(&@~^+++#@) :: [CornerPointsWithDegreesAndStl] -> (CornerPointsWithDegrees -> [CornerPointsWithDegreesAndStl]) -> [CornerPointsWithDegreesAndStl]
cornerPointsWithDegreesAndStlList  &@~^+++#@ f =  (f $ _cornerPoints $ head cornerPointsWithDegreesAndStlList) : cornerPointsWithDegreesAndStlList


fx ::  [CornerPointsWithDegrees] -> [FacesWithRange] -> [CornerPointsWithDegreesAndStl
fx     cornerPointsWithDegreesList facesWithRange   =
         CornerPointsWithDegreesAndStl  (cornerPointsWithDegreesList |@~+++#@| (id )) facesWithRange
-}
