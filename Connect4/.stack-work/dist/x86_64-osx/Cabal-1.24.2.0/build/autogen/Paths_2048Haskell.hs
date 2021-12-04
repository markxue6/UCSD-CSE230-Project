{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_2048Haskell (
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

bindir     = "/Users/shuchizhang/Desktop/Connect4/.stack-work/install/x86_64-osx/997be95876816b573d81de9d29a4fc27fa0ea0c8ba6f091443caacb90f407f0e/8.0.2/bin"
libdir     = "/Users/shuchizhang/Desktop/Connect4/.stack-work/install/x86_64-osx/997be95876816b573d81de9d29a4fc27fa0ea0c8ba6f091443caacb90f407f0e/8.0.2/lib/x86_64-osx-ghc-8.0.2/2048Haskell-0.1.0.0"
dynlibdir  = "/Users/shuchizhang/Desktop/Connect4/.stack-work/install/x86_64-osx/997be95876816b573d81de9d29a4fc27fa0ea0c8ba6f091443caacb90f407f0e/8.0.2/lib/x86_64-osx-ghc-8.0.2"
datadir    = "/Users/shuchizhang/Desktop/Connect4/.stack-work/install/x86_64-osx/997be95876816b573d81de9d29a4fc27fa0ea0c8ba6f091443caacb90f407f0e/8.0.2/share/x86_64-osx-ghc-8.0.2/2048Haskell-0.1.0.0"
libexecdir = "/Users/shuchizhang/Desktop/Connect4/.stack-work/install/x86_64-osx/997be95876816b573d81de9d29a4fc27fa0ea0c8ba6f091443caacb90f407f0e/8.0.2/libexec"
sysconfdir = "/Users/shuchizhang/Desktop/Connect4/.stack-work/install/x86_64-osx/997be95876816b573d81de9d29a4fc27fa0ea0c8ba6f091443caacb90f407f0e/8.0.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "2048Haskell_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "2048Haskell_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "2048Haskell_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "2048Haskell_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "2048Haskell_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "2048Haskell_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
