module Examples.Adaptors.HockeyStick where

import  Primitives.Cubical(rectangularCube)


{-
Adaptor that goes inside of the plastic hockey stick.
It will have the center tapped out for a thread that connects to hose.
-}

--get outer faces of outerWall, to add to 

--get outer faces of innerWall, and turn into inner faces

outerWall =
  [rectangularCube height_ width_ length_]

innerWall =
  [rectangularCube height_ 2 2]
  
--height width length
height_ = 30
width_ = 12
length_ = 14
