module Helpers.List((++:)) where
-- |Appends a list onto a list of lists.
-- Known uses: building up a list of Faces to be zipped to list of Cornerpoints using (+++^) for createing stl output

infix 6 ++:


(++:) :: [[a]] -> [a] -> [[a]]
multiList ++: singleList = reverse $ singleList : (reverse multiList)

