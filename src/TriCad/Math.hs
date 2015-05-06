module TriCad.Math(degreesToRadians,
radiansToDegrees,
--upperZValueOfWedge,
--upperYValueOfWedge,
sinDegrees,
cosDegrees) where

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

