module CornerPoints.FaceExtractionWithDegrees (extractFrontFace) where
import qualified CornerPoints.FaceExtraction as FaceEx (extractFrontFace)
import CornerPoints.CornerPointsWithDegrees(CornerPointsWithDegrees(..), DegreeRange(..))


extractFrontFace :: CornerPointsWithDegrees -> CornerPointsWithDegrees
extractFrontFace (CubesWithStartEndDegrees cube' (DegreeRange startDegree' endDegree')) =
   FrontFaceWithStartEndDegrees  (FaceEx.extractFrontFace cube') (DegreeRange startDegree' endDegree')




