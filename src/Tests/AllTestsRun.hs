module Tests.AllTestsRun(runAllTestsDo) where
import Tests.CornerPointsDebugTest(cornerPointsDebugTestDo)
import Tests.CornerPointsTest(cornerPointsTestDo )
import Tests.MathTest(mathTestDo)
import Tests.MathPolarTest(mathPolarTestDo)
import Tests.MathTest(mathTestDo)
import Tests.ParseTest(parseTestDo)
import Tests.ScannerTest(scannerTestDo)
import Tests.JsonTest(jsonTestDo)
import Tests.ParseRawToADTTest(parseRawToADTTestDo)  

runAllTestsDo = do
  putStrLn "corner points debug test"
  cornerPointsDebugTestDo
  putStrLn "corner points test"
  cornerPointsTestDo
  putStrLn "math test"
  mathTestDo
  putStrLn "math polar test"
  mathPolarTestDo
  putStrLn "parse test"
  parseTestDo
  putStrLn "json test"
  jsonTestDo
  putStrLn "parse raw to adt test"
  parseRawToADTTestDo
