module Paths_Tricad (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,1,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/heath/haskell_projects/Tricad/.cabal-sandbox/bin"
libdir     = "/home/heath/haskell_projects/Tricad/.cabal-sandbox/lib/x86_64-linux-ghc-7.8.3/Tricad-0.1.0.0"
datadir    = "/home/heath/haskell_projects/Tricad/.cabal-sandbox/share/x86_64-linux-ghc-7.8.3/Tricad-0.1.0.0"
libexecdir = "/home/heath/haskell_projects/Tricad/.cabal-sandbox/libexec"
sysconfdir = "/home/heath/haskell_projects/Tricad/.cabal-sandbox/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Tricad_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Tricad_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "Tricad_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Tricad_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Tricad_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
