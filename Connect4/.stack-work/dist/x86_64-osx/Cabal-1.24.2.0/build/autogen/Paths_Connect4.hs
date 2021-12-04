{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_Connect4 (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/shuchizhang/Desktop/UCSD-CSE230-Project/Connect4/.stack-work/install/x86_64-osx/997be95876816b573d81de9d29a4fc27fa0ea0c8ba6f091443caacb90f407f0e/8.0.2/bin"
libdir     = "/Users/shuchizhang/Desktop/UCSD-CSE230-Project/Connect4/.stack-work/install/x86_64-osx/997be95876816b573d81de9d29a4fc27fa0ea0c8ba6f091443caacb90f407f0e/8.0.2/lib/x86_64-osx-ghc-8.0.2/Connect4-0.1.0.0"
dynlibdir  = "/Users/shuchizhang/Desktop/UCSD-CSE230-Project/Connect4/.stack-work/install/x86_64-osx/997be95876816b573d81de9d29a4fc27fa0ea0c8ba6f091443caacb90f407f0e/8.0.2/lib/x86_64-osx-ghc-8.0.2"
datadir    = "/Users/shuchizhang/Desktop/UCSD-CSE230-Project/Connect4/.stack-work/install/x86_64-osx/997be95876816b573d81de9d29a4fc27fa0ea0c8ba6f091443caacb90f407f0e/8.0.2/share/x86_64-osx-ghc-8.0.2/Connect4-0.1.0.0"
libexecdir = "/Users/shuchizhang/Desktop/UCSD-CSE230-Project/Connect4/.stack-work/install/x86_64-osx/997be95876816b573d81de9d29a4fc27fa0ea0c8ba6f091443caacb90f407f0e/8.0.2/libexec"
sysconfdir = "/Users/shuchizhang/Desktop/UCSD-CSE230-Project/Connect4/.stack-work/install/x86_64-osx/997be95876816b573d81de9d29a4fc27fa0ea0c8ba6f091443caacb90f407f0e/8.0.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Connect4_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Connect4_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "Connect4_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "Connect4_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Connect4_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Connect4_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
