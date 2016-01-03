{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ParallelListComp #-}
{- | This is the same as CornerPoints.VerticalFaces, except it uses CornerPointsWithDegreeInfo instead of CornerPoints.
     That is to say that all the CornerPoints have Degree information attached to them.
     The purpose is to make it much cleaner to create stl from CornerPoints.-}
module CornerPoints.CornerPointsWithDegrees(CornerPointsWithDegrees(..), (+++~), (+++~>), (|+++~|), (@~+++#@),(@~+++@),(|@~+++@|), (|@~+++#@|),
                                            cubeIsWithinDegreeRange, DegreeRange(..), newCornerPointsWithDegrees, newCornerPointsWith10DegreesList,
                                            newCornerPointsWith5DegreesList) where


import CornerPoints.CornerPoints(CornerPoints(..),(@+++#@),(+++), (+++>))
import CornerPoints.Degree(Degree(..))
{- |Wrap a CornerPoints to include Degree information.

This is based on, like all CornerPoints work, on a radial structure that rotates clockwise.

A RigthFace is always the trailing side of the cube, and so has a start degree.

A LeftFace is always the leadiong side of the cube, and so has an end degree.

Top, Bottom, Front, and Back Faces have to have both start and end degrees.

Eg: The first cube that goes from 0 to 10 degrees will have start degree = 0 end end degree = 10-}

--make signatures more readable
--type Degree = Double

-- ToDo: Add in the rest of the faces.
--ToDo: remove the 's' from Bottom/TopFaces & CubesWith

data CornerPointsWithDegrees =
    CubesWithStartEndDegrees {_cube::CornerPoints, _degreeRange::DegreeRange}
  | FrontFaceWithStartEndDegrees {_frontFace::CornerPoints, _degreeRange::DegreeRange}
  | BackFaceWithStartEndDegrees {_backFace::CornerPoints, _degreeRange::DegreeRange}
  | LeftFaceWithDegrees {_leftFace::CornerPoints, _degree::Double}
  | RightFaceWithDegrees {_rightFace::CornerPoints, _degree::Double}
  | BottomFacesWithDegrees {_bottomFace::CornerPoints, _degreeRange::DegreeRange}
  | TopFacesWithDegrees {_topFace::CornerPoints, _degreeRange::DegreeRange}
  deriving(Show, Eq)

data DegreeRange = DegreeRange {_startDegree::Degree, _endDegree::Degree}
     deriving(Show, Eq)

{- |Instantiate a new CornerPointsWithDegrees based on CornerPoints constructor, and a start and end degree.
Constructors such as LeftFace only require a single degree. Use the fst (start,end).
This is used as a base for functions that set the degree intervals, such as every 10 degrees.-}
-- ToDo: Finish pattern matching for all other CornerPoints constructors.
newCornerPointsWithDegrees :: CornerPoints -> (Degree, Degree) -> CornerPointsWithDegrees
newCornerPointsWithDegrees (TopFace b2 f2 b3 f3 ) (start,end) = TopFacesWithDegrees (TopFace b2 f2 b3 f3 ) (DegreeRange start end)
newCornerPointsWithDegrees (CubePoints f1 f2 f3 f4 b1 b2 b3 b4) (start,end) = CubesWithStartEndDegrees  (CubePoints f1 f2 f3 f4 b1 b2 b3 b4) (DegreeRange start end)

{- |Used to instantiate a new [CornerPointsWithDegrees] for a full 360 deg shape starting at 0 to 360 degrees at 10 degree intervals..-}
newCornerPointsWith10DegreesList :: [CornerPoints] -> [CornerPointsWithDegrees] 
newCornerPointsWith10DegreesList cornerPointsList =
  let
    degrees360Tuples :: [(Degree, Degree)]
    degrees360Tuples = zipWith (,) [0,10..350] [10,20..360]
  in
   (zipWith newCornerPointsWithDegrees cornerPointsList  degrees360Tuples)

{- |Used to instantiate a new [CornerPointsWithDegrees] for a full 360 deg shape starting at 0 to 360 degrees at 5 degree intervals..-}
newCornerPointsWith5DegreesList :: [CornerPoints] -> [CornerPointsWithDegrees] 
newCornerPointsWith5DegreesList cornerPointsList =
  let
    degrees360Tuples :: [(Degree, Degree)]
    degrees360Tuples = zipWith (,) [0,5..355] [5,10..360]
  in
   (zipWith newCornerPointsWithDegrees cornerPointsList  degrees360Tuples)

   

{- |Filter a list of CornerPointsWithDegrees to get only those within a given degree range. -}
cubeIsWithinDegreeRange :: DegreeRange -> [CornerPointsWithDegrees] -> [CornerPointsWithDegrees]
cubeIsWithinDegreeRange    (DegreeRange start     end) cubesWithStartEndDegrees  =
        let isWithinRange :: Degree -> Degree -> CornerPointsWithDegrees -> Bool
            isWithinRange    start     end (CubesWithStartEndDegrees _ (DegreeRange startDegree' endDegree')) = 
               startDegree' >= start && endDegree' <= end

        in 
          filter (isWithinRange start end) cubesWithStartEndDegrees


--genStlFromCubesWithDegreesAndFaces :: 


(+++~) :: CornerPointsWithDegrees ->           CornerPointsWithDegrees               -> CornerPointsWithDegrees
(RightFaceWithDegrees faceRight rightDeg) +++~ (LeftFaceWithDegrees faceLeft leftDeg) =
                            CubesWithStartEndDegrees (faceRight +++ faceLeft ) (DegreeRange rightDeg leftDeg)
(CubesWithStartEndDegrees cube (DegreeRange startDegree' endDegree')) +++~ (LeftFaceWithDegrees faceLeft leftDeg) =
                            CubesWithStartEndDegrees (cube +++ faceLeft) (DegreeRange endDegree' leftDeg)


{-Add a CornerPoints to a CornerPointsWithDegrees. The start/end degrees will come form the CornerPointsWithDegrees
Good for adding things like top/bottom faces, so as to know for sure, which start/end degrees is used.-}
(@~+++@) :: CornerPointsWithDegrees -> CornerPoints -> CornerPointsWithDegrees
(TopFacesWithDegrees cube' (DegreeRange startDegree' endDegree')) @~+++@ cPnt = (CubesWithStartEndDegrees (cube' +++ cPnt) (DegreeRange startDegree' endDegree'))
(CubesWithStartEndDegrees cube' (DegreeRange startDegree' endDegree')) @~+++@ cPnt = (CubesWithStartEndDegrees (cube' +++ cPnt) (DegreeRange startDegree' endDegree'))
{-list version of (@~+++@)-}
(|@~+++@|) :: [CornerPointsWithDegrees] -> [CornerPoints] -> [CornerPointsWithDegrees]
cornerPointsWithDegrees |@~+++@| cornerPoints = zipWith (@~+++@) cornerPointsWithDegrees cornerPoints


{-Same as +++> but with degree info attached.-}
(+++~>) :: CornerPointsWithDegrees -> [CornerPointsWithDegrees] -> [CornerPointsWithDegrees]
x+++~> xs = tail $ scanl (+++~) x xs

{- |Add [CornerPointsWithDegrees] to [CornerPointsWithDegrees].
-}
(|+++~|) :: [CornerPointsWithDegrees] -> [CornerPointsWithDegrees] -> [CornerPointsWithDegrees]
c1 |+++~| c2 = zipWith (+++~) c1 c2

{- |Apply a function to the CornerPoints inside of a CornerPointsWithDerees, +++ it to the original CornerPoints,
    and return a new CornerPointsWithDerees with the same degeee info, and a modified CornerPoints-}
(@~+++#@) :: CornerPointsWithDegrees -> (CornerPoints -> CornerPoints) -> CornerPointsWithDegrees
(CubesWithStartEndDegrees cube' (DegreeRange startDegree' endDegree')) @~+++#@ f =
  (CubesWithStartEndDegrees (cube' +++ (f cube')) (DegreeRange startDegree' endDegree'))


(|@~+++#@|) :: [CornerPointsWithDegrees] -> (CornerPoints -> CornerPoints) -> [CornerPointsWithDegrees]
cornerPointsWithDegreesList |@~+++#@| f = map (@~+++#@ f) cornerPointsWithDegreesList







