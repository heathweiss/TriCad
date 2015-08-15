{-# LANGUAGE OverloadedStrings #-}
module Scan.ParseAtto(getPixelRow, getPixelRowMulti, getDegree, getDegreeScan, DegreeScan(..)
                     , getMultiDegreeScan, MultiDegreeScan(..)) where
import Data.Word
import Data.Attoparsec.Char8
import Control.Applicative
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BS

logFile :: FilePath
logFile = "src/Data/parseAttoCanned.raww"

spaceSkip = many $ satisfy $ inClass [ ' ' , '\t' ]

getPixelRowBase :: Parser Double
getPixelRowBase = do
  spaceSkip
  pix1 <- double
  return pix1

getPixelRow :: Parser [Double]
getPixelRow = do
  many $ getPixelRowBase

getPixelRowMulti :: Parser [[Double]]
getPixelRowMulti = do
  sepBy getPixelRow (char ';')

getDegree :: Parser Double
getDegree = do
  degree <- double
  return degree


data DegreeScan = DegreeScan {degree::Double, radii::[[Double]]}
  deriving (Show, Eq)

getDegreeScan :: Parser DegreeScan
getDegreeScan = do
  degree <- getDegree
  radii <- getPixelRowMulti
  return $ DegreeScan degree radii


data MultiDegreeScan = MultiDegreeScan {name::String, degrees::[DegreeScan]}

data ScanName = ScanName {scanName::String}

getName :: Parser ScanName
getName = do
  name <- Data.Attoparsec.Char8.takeWhile (/=' ')
  return $ ScanName $ BS.unpack  name

getMultiDegreeScan :: Parser MultiDegreeScan
getMultiDegreeScan = do
  scanName' <- getName
  degreeScans <- sepBy  getDegreeScan (char '$')
  return MultiDegreeScan {name=(scanName scanName'), degrees=degreeScans}
