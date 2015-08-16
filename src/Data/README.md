files are used by:

parseAttoCanned.raww
-string of raw data for parsing into Scan object via attoparse in Scan.ParseAtto.

scan.json
-a small Scan object written as json
-used by src/Scan/Json.hs

temp.stl
-the stl file output from TriCad.StlFileWriter
-so any code that outputs the final stl file, (over)writes this file.

debug.txt
-the output from TriCad.StlFileWriter.writeStlDebugToFile
-any shape module which outputs debug info, (over)writes to this file

scanRawDataWitDegrees.raww
-the output from the C++ opencv, for the raw image data.
-It has the extra degree info attached
-used by src/MiscShapes/ScanRaw.hs for the ParseAtto