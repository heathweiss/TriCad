module Tests.ParseRawTest (parseRawDo) where
import Test.HUnit

parseRawDo = do
  runTestTT hello

hello = TestCase $ assertEqual
  "hello from ParseRawTest"
  (1)
  (1)
