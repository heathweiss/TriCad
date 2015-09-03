module TriCad.Math(degreesToRadians,
radiansToDegrees,
--upperZValueOfWedge,
--upperYValueOfWedge,
sinDegrees,
cosDegrees,
atanDegrees) where
import qualified Data.Decimal as D


{-
If degrees < 0, make them positive
-}
degreesToRadians:: Double -> Double
degreesToRadians degrees
  | degrees < 0 = degreesToRadians $ negate degrees
  | otherwise = degrees * pi/180


radiansToDegrees :: Double -> Double
radiansToDegrees radians = radians * 180/pi

{-
Prelude sin uses radians. This puts a wrapper around it to pass in degrees.
-}
sinDegrees :: Double -> Double
sinDegrees degrees = sin $ degreesToRadians degrees
                   
{-
Prelude cos uses radians. This puts a wrapper around it to pass in degrees.
-}

cosDegrees :: Double -> Double
cosDegrees degrees = cos $ degreesToRadians degrees

--get the angle in degrees, using atan.
atanDegrees :: Double -> Double -> Double
atanDegrees opp adj = radiansToDegrees $ atan $ opp/adj
