module Examples.Scan.WalkerSocket() where
import Scan.ParseJuicy(process10DegreeImagesToMultiDegreeRadii, getRedLaserLineSingleImage)
import Data.Word(Word8)

{- |Process all 36 images, reducing down to the red laser line.
-}
processScanImagesToMultiDegreeRadiiOfRedLaserLine = process10DegreeImagesToMultiDegreeRadii (getRedLaserLineSingleImage redLaserLine)

type TargetValue = Word8
redLaserLine :: TargetValue
redLaserLine = 190
