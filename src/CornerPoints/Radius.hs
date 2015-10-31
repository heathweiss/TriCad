module CornerPoints.Radius(Radius(..)) where



{-|
Represents a radius of a circular shape, which is what all shapes in math polar are created from.
-}
data Radius = Radius {radius :: Double}
             | DownRadius {radius :: Double} -- | Radius slopes down from origin z-axis. 
             | UpRadius {radius :: Double}
   deriving (Show, Eq)
