{-# LANGUAGE OverloadedStrings #-}
module Tests.JsonTest where
import Test.HUnit
import TriCad.MathPolar(Radius())
import Scan.Json(Degree(..))
import Data.Aeson
import qualified Data.ByteString.Lazy as BS
import TriCad.MathPolar(Radius(..))


jsonTestDo = do
  runTestTT radiusDecodeTest
  runTestTT radiusEncodeTest
  runTestTT degreeEncodeTest
  runTestTT degreeDecodeTest

degreeEncodeTest = TestCase $ assertEqual
  "encode radius"
  ("{\"radii\":[{\"radius\":12},{\"radius\":120}],\"degree\":0}")
  (encode (Degree {degree=0, radii=[Radius {radius=12}, Radius {radius=120}]}))

degreeDecodeTest = TestCase $ assertEqual
  "decode degree"
  (Just (Degree 0 [Radius 12, Radius 120]))
  ((decode  "{\"radii\":[{\"radius\":12},{\"radius\":120}],\"degree\":0}") :: Maybe Degree)

radiusDecodeTest = TestCase $ assertEqual
  "decode radius"
  (Just (Radius 12))
  ((decode  "{\"radius\":12}") :: Maybe Radius)


radiusEncodeTest = TestCase $ assertEqual
  "encode a radius"
  ( ( "{\"radius\":12}"))
  (encode $  (Radius {radius=12}) )
