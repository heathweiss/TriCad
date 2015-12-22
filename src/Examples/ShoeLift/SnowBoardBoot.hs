{-# LANGUAGE ParallelListComp #-}
{- |
Snowboard boot.

A single piece lift, done by measuring the radii off of graph paper.
-}
module Examples.ShoeLift.SnowBoardBoot () where

import Scan.ParseJuicy( getRedLaserLineSingleImage, removeLeftOfCenterPixels, getThePixelsRightOfCenter,  reduceScanRows, reduceScan,
                       calculateRadiusFrom, calculatePixelsPerMillmeter)
import Data.Word(Word8)
import CornerPoints.Radius(resetMultiDegreeRadiiIfNullWithPreviousValue, MultiDegreeRadii(..), SingleDegreeRadii(..), Radius(..), )
import CornerPoints.Points(Point(..))
import qualified Data.ByteString.Lazy as BL
import Data.Aeson
import Helpers.DSL (ofThe, forThe, andThen, adjustedFor, andThe)
import CornerPoints.VerticalFaces(createHorizontallyAlignedCubesNoSlope)
import Stl.StlCornerPoints((|+++^|), (||+++^||), Faces(..))
import Stl.StlBase (StlShape(..), newStlShape)
import Stl.StlFileWriter(writeStlToFile)
import CornerPoints.CornerPoints(CornerPoints(..), (+++), (|+++|), (|@+++#@|), (&+++#@), CornerPointsBuilder(..))



bootRadius = map Radius
  [ 126.4,--0
    126.0,--5
    124.7,--10
    119.2,--15
    109.1,--20
    93.9,--25
    81.5,--30
    71.2,--35
    64.0,--40
    57.3,--45
    52.8,--50
    49.0,--55
    46.6,--60
    45.6,--65
    44.8,--70
    44.7,--75
    44.4,--80
    44.7,--85
    44.2,--90
    46.0,--95
    47.6,--100
    48.7,--105
    51.0,--110
    54.1,--115
    58.3,--120
    63.0,--125
    67.4,--130
    72.9,--135
    79.5,--140
    88.2,--145
    96.4,--150
    104.7,--155
    113.8,--160
    121.5,--165
    125.7,--170
    130.0,--175
    131.9,--180
    133.3,--185
    134.4,--190
    132.9,--195
    130.2,--200
    122.6,--205
    111.2,--210
    96.9,--215
    87.7,--220
    76.7,--225
    69.2,--230
    63.5,--235
    58.4,--240
    52.6,--245
    48.8,--250
    46.0,--255
    43.8,--260
    41.4,--265
    40.4,--270
    39.7,--275
    38.5,--280
    38.5,--285
    38.5,--290
    39.5,--295
    40.8,--300
    42.3,--305
    44.7,--310
    50.7,--315
    57.6,--320
    66.8,--325
    80.8,--330
    96.2,--335
    112.0,--340
    122.7,--345
    127.5,--350
    126.8,--355
    126.4--360

  ]

--Where the lift meets the boot.
bootCubes = newCornerPointsWith5DegreesBuilder
  ()
