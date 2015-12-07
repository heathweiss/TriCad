module CornerPoints.FaceConversionsWithDegrees (backFaceFromFrontFace) where 
import qualified CornerPoints.FaceConversions as FaceEx (backFaceFromFrontFace, upperFaceFromLowerFace, lowerFaceFromUpperFace )
import CornerPoints.CornerPointsWithDegrees(CornerPointsWithDegrees(..), DegreeRange(..))
                                            
backFaceFromFrontFace :: CornerPointsWithDegrees -> CornerPointsWithDegrees
backFaceFromFrontFace (FrontFaceWithStartEndDegrees cube' (DegreeRange startDegree' endDegree')) =
  BackFaceWithStartEndDegrees (FaceEx.backFaceFromFrontFace cube')  (DegreeRange startDegree' endDegree')
