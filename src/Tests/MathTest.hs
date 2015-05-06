module Tests.MathTest(mathTestDo) where
import TriCad.Math
import Test.HUnit


mathTestDo = do

  putStrLn "cosDegrees" 
  runTestTT cosDegreesTest
  
--note how it does not come out to 0. This is due to rounding errors and/or irrational # errors of pi.
cosDegreesTest = TestCase $ assertEqual 
  "cosDegrees" (1.102182119232618e-15) ((cosDegrees 90) * 18  )




