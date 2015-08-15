{-# LANGUAGE OverloadedStrings #-}
module Scan.Json() where
import TriCad.MathPolar(Radius(..),SingleDegreeScan(..), Scan(..))
import Data.Aeson
import Control.Applicative
import Control.Monad
import Data.Text as T
--import System.IO
import qualified Data.ByteString.Lazy as BL
{-
Write and parse Scan datatype as json using Data.Aeson package.

Known uses:
Once the raw data for an image has been process, save it to file or mongo as json, so it does not
have to be processed again.
-}

---------------------------- Scan -------------------------------------------
{-
name:
Could come in handy later on, if I want to store in Mongo.
If not for mongo, why not just use the file name.
The stlBuilder could use it for naming the stl object, which is required by the stl format.
-} 
instance ToJSON Scan where
  toJSON (Scan name degrees) = object ["name" .= name, "degrees" .= degrees]

 
instance FromJSON Scan where
  parseJSON (Object v) = Scan <$>
                         v .: "name" <*>
                         v .: "degrees"
  parseJSON _          = mzero

--------------------------- Degree ------------------------------------------------
instance ToJSON SingleDegreeScan where
  toJSON (SingleDegreeScan degree radii) = object ["degree" .= degree, "radii" .=radii]

instance FromJSON SingleDegreeScan where
  parseJSON (Object v) = SingleDegreeScan <$>
                         v .: "degree" <*>
                         v .: "radii"
  parseJSON _          = mzero




  
---------------------------- Radius ------------------------------------
{-
I have only used the Radius constructor, and not the Up/DownRadius constructors.
Those 2 only get used later on, when dealing with slopes.
-}
instance FromJSON Radius where
  parseJSON (Object v) = Radius <$>
                         v .: "radius"
  parseJSON _          = mzero


instance ToJSON Radius where
  toJSON (Radius radius) = object ["radius" .= radius]



--------------------------------test some file output-----------------------------
{-
create/overwrites the file haskell_project/TriCad/scan.json
-}
writeToFileScan = do
  BL.writeFile "src/Data/scan.json" 
     (encode (Scan
               { name = "myScan",
                 degrees =
                  [
                   SingleDegreeScan {degree=0, radii=[Radius {radius=12}, Radius {radius=120}]},
                   SingleDegreeScan {degree=10, radii=[Radius {radius=120}, Radius {radius=1200}]}
                  ]
               }
          )
     )

{-
depends on the file  written with writeToFileScan
-}
readFromFileScan = do
  contents <- BL.readFile "src/Data/scan.json"
  case (decode contents) of
      Just (Scan name degrees) -> print $ show $  (Scan name degrees)
      Nothing                  -> putStrLn "Nothing"
  
  
