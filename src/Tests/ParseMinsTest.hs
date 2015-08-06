module Tests.ParseMinsTest(parseMinsDo) where
import Test.HUnit

parseMinsDo = do
  runTestTT hello

hello = TestCase $ assertEqual
  "hello from parse mins"
  (1)
  (1)
  
