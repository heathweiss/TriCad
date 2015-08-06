module Tests.ParseTest (parseTestDo) where
import Test.HUnit

parseTestDo = do
  runTestTT hello

hello = TestCase $ assertEqual
 "hello test"
 (1)
 (1)
