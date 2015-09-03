module Adaptors.HockeyStick where

import  TriCad.Shapes.Cubical(rectangularCube)


{-
Adaptor that goes inside of the plastic hockey stick.
It will have the center tapped out for a thread that connects to hose.
-}

--get outer faces of outerWall, to add to 

--get outer faces of innerWall, and turn into inner faces

outerWall =
  [rectangularCube height width length]

innerWall =
  [rectangularCube height 2 2]
  
--height width length
height = 30
width = 12
length = 14
