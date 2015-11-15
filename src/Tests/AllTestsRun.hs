module Tests.AllTestsRun(runAllTestsDo) where
import Tests.CornerPointsDebugTest(cornerPointsDebugTestDo)
import Tests.CornerPointsTest(cornerPointsTestDo )
import Tests.MathTest(mathTestDo)
import Tests.MathPolarTest(mathPolarTestDo)
import Tests.MathTest(mathTestDo)
import Tests.JsonTest(jsonTestDo)
import Tests.HorizontalFacesTest (horizontalFacesTestDo)
import Tests.VerticalFacesTest(verticalFacesTestDo)
import Tests.ParseJuicyTest (parseJuicyTestDo)
import Tests.ScanFilterTest (scanFilterTestDo)
import Tests.ListHelpersTest(listHelperTestDo)
import Tests.CornerPointsCreateTest(cornerPointsCreateTestDo )

runAllTestsDo = do
  putStrLn "corner points debug test"
  cornerPointsDebugTestDo
  putStrLn "corner points test"
  cornerPointsTestDo
  putStrLn "math test"
  mathTestDo
  putStrLn "math polar test"
  mathPolarTestDo
  putStrLn "json test"
  jsonTestDo
  putStrLn "horizontal faces test"
  horizontalFacesTestDo
  putStrLn "vertical faces test"
  verticalFacesTestDo
  putStrLn "parseJuicy test"
  parseJuicyTestDo
  putStrLn "scan.filter test"
  scanFilterTestDo
  putStrLn "list helpers test"
  listHelperTestDo
  putStrLn "cornerpoints create test"
  cornerPointsCreateTestDo
