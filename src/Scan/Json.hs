{-# LANGUAGE OverloadedStrings #-}
module Scan.Json(Degree(..)) where
import TriCad.MathPolar(Radius(..))
import Data.Aeson
import Control.Applicative
import Control.Monad
import Data.Text as T
--import qualified Data.ByteString.Lazy as BL

data Degree = Degree {degree::Int, radii::[Radius]}
     deriving (Show, Eq)

instance ToJSON Degree where
  toJSON (Degree degree radii) = object ["degree" .= degree, "radii" .=radii]

instance FromJSON Degree where
  parseJSON (Object v) = Degree <$>
                         v .: "degree" <*>
                         v .: "radii"
  parseJSON _          = mzero
  

instance FromJSON Radius where
  parseJSON (Object v) = Radius <$>
                         v .: "radius"
  parseJSON _          = mzero


instance ToJSON Radius where
  toJSON (Radius radius) = object ["radius" .= radius]
