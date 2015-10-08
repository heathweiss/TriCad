{-|
Module:  Types
Description: Supply common types

Supply common types that are used by several modules. Makes it easy to avoid circular references.
-}

module TriCad.Types (PixelIndice(..), PixelValue(..), Name(..))where

-- |The indice(position) of a pixel in a the image taken for a scan.
--The indice, as a offset from the horizontal center, is the radius, though it will still need to be converted to a distance.
type PixelIndice = Int

-- |Actual pixel value, as captured by the scanner/camera.
type PixelValue = Double

-- |Name of anything.
-- Known uses: Required name in stl file format. Name used for storing scans.
type Name = String
 
