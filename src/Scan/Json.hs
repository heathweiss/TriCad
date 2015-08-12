{-# LANGUAGE OverloadedStrings #-}
module Scan.Json(Degree(..), Scan(..)) where
import TriCad.MathPolar(Radius(..))
import Data.Aeson
import Control.Applicative
import Control.Monad
import Data.Text as T
--import System.IO
import qualified Data.ByteString.Lazy as BL
{-
Write and parse scans as json using Data.Aeson package.

Started off with MathPolar.Radius
-The radius measurement used by MathPolar to create shapes using Radius and degrees of a circular object.

Have added:
-Degree:
  -A degree at which a scan occurred, and all the assoc'd [Radius]. As scanning happens on the vertical
   axis, a single degree will contain many Radius.
  -Should probably be moved into a TriCad module, along with Radius and Scan
-Scan:
  -A [Degree] which gives all the Radius for an entire scan of an object.

Future things:
-Figure out where to put Radius, Degree, and Scan datatypes and the To/FromJSON instance declarations.
-}

---------------------------- Scan -------------------------------------------
{-
name:
Could come in handy later on, if I want to store in Mongo.
If not for mongo, why not just use the file name.
The stlBuilder could use it for naming the stl object, which is required by the stl format.
-}
data Scan = Scan {name::String, degrees::[Degree]}
          deriving (Show, Eq)

instance ToJSON Scan where
  toJSON (Scan name degrees) = object ["name" .= name, "degrees" .= degrees]

  
instance FromJSON Scan where
  parseJSON (Object v) = Scan <$>
                         v .: "name" <*>
                         v .: "degrees"
  parseJSON _          = mzero

--------------------------- Degree ------------------------------------------------
data Degree = Degree {degree::Double, radii::[Radius]}
     deriving (Show, Eq)

instance ToJSON Degree where
  toJSON (Degree degree radii) = object ["degree" .= degree, "radii" .=radii]

instance FromJSON Degree where
  parseJSON (Object v) = Degree <$>
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
  BL.writeFile "scan.json" 
     (encode (Scan
               { name = "myScan",
                 degrees =
                  [
                   Degree {degree=0, radii=[Radius {radius=12}, Radius {radius=120}]},
                   Degree {degree=10, radii=[Radius {radius=120}, Radius {radius=1200}]}
                  ]
               }
          )
     )

{-
depends on the file haskell_project/TriCad/scan.json which was written with writeToFileScan
-}
readFromFileScan = do
  contents <- BL.readFile "/home/heath/haskell_projects/Tricad/scan.json"
  case (decode contents) of
      Just (Scan name degrees) -> print $ show $  (Scan name degrees)
      Nothing                  -> putStrLn "Nothing"
  
  
