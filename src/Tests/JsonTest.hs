{-# LANGUAGE OverloadedStrings #-}
module Tests.JsonTest where
import Test.HUnit
import TriCad.MathPolar(Radius())
import Scan.Json
import Data.Aeson
import qualified Data.ByteString.Lazy as BS
import TriCad.MathPolar(Radius(..),Degree(..),Scan(..))


jsonTestDo = do
  runTestTT radiusDecodeTest
  runTestTT radiusEncodeTest
  runTestTT degreeEncodeTest
  runTestTT degreeDecodeTest
  runTestTT scanEncodeTest
  runTestTT scanDecodeTest

---------------------------------scan----------------------------------------------
scanEncodeTest = TestCase $ assertEqual
  "encode scan"
  ( "{\"degrees\":[{\"radii\":[{\"radius\":12},{\"radius\":120}],\"degree\":0.0},{\"radii\":[{\"radius\":120},{\"radius\":1200}],\"degree\":10}],\"name\":\"myScan\"}" )

  (encode (Scan
               { name = "myScan",
                 degrees =
                  [
                   Degree {degree=0, radii=[Radius {radius=12}, Radius {radius=120}]},
                   Degree {degree=10, radii=[Radius {radius=120}, Radius {radius=1200}]}
                  ]
               }
          )
  )



scanDecodeTest = TestCase $ assertEqual
  "decode scan"
  (Just
       (Scan
               { name = "myScan",
                 degrees =
                  [
                   Degree {degree=0, radii=[Radius {radius=12}, Radius {radius=120}]},
                   Degree {degree=10, radii=[Radius {radius=120}, Radius {radius=1200}]}
                  ]
               }
          )
  )
  ((decode  "{\"degrees\":[{\"radii\":[{\"radius\":12},{\"radius\":120}],\"degree\":0},{\"radii\":[{\"radius\":120},{\"radius\":1200}],\"degree\":10}],\"name\":\"myScan\"}"     ) :: Maybe Scan)

------------------------- degree -------------------------------------------------
degreeEncodeTest = TestCase $ assertEqual
  "encode radius"
  ("{\"radii\":[{\"radius\":12},{\"radius\":120}],\"degree\":0.0}")
  (encode (Degree {degree=0, radii=[Radius {radius=12}, Radius {radius=120}]}))

degreeDecodeTest = TestCase $ assertEqual
  "decode degree"
  (Just (Degree 0 [Radius 12, Radius 120]))
  ((decode  "{\"radii\":[{\"radius\":12},{\"radius\":120}],\"degree\":0}") :: Maybe Degree)





-------------------------- radius ----------------------------------
radiusDecodeTest = TestCase $ assertEqual
  "decode radius"
  (Just (Radius 12))
  ((decode  "{\"radius\":12}") :: Maybe Radius)


radiusEncodeTest = TestCase $ assertEqual
  "encode a radius"
  ( ( "{\"radius\":12}"))
  (encode $  (Radius {radius=12}) )
