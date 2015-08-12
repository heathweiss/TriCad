files are used by:

sample.raww
-a small string of raw scan data, that has the degree info attached.
-src/Scan/Parse/RawToADT.hs uses it for testing.

scan.json
-a small Scan object written as json
-used by src/Scan/Json.hs

temp.stl
-the stl file output from TriCad.StlFileWriter
-so any code that outputs the final stl file, (over)writes this file.

debug.txt
-the output from TriCad.StlFileWriter.writeStlDebugToFile
-any shape module which outputs debug info, (over)writes to this file

scanRawData.raww
-the output from the C++ opencv, for the raw image data.
-It does not have the extra degree info attached
-used by src/MiscShapes/ScanRaw.hs

scanRawDataWitDegrees.raww
-the output from the C++ opencv, for the raw image data.
-It has the extra degree info attached
-used by src/MiscShapes/ScanRaw.hs 