{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_microc (
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
version = Version [1,0,0,1] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/hurou/Dropbox/haskell/microc/.stack-work/install/x86_64-linux/lts-9.11/8.0.2/bin"
libdir     = "/home/hurou/Dropbox/haskell/microc/.stack-work/install/x86_64-linux/lts-9.11/8.0.2/lib/x86_64-linux-ghc-8.0.2/microc-1.0.0.1-3047B3BsXJw1BTxkqIoVAy"
dynlibdir  = "/home/hurou/Dropbox/haskell/microc/.stack-work/install/x86_64-linux/lts-9.11/8.0.2/lib/x86_64-linux-ghc-8.0.2"
datadir    = "/home/hurou/Dropbox/haskell/microc/.stack-work/install/x86_64-linux/lts-9.11/8.0.2/share/x86_64-linux-ghc-8.0.2/microc-1.0.0.1"
libexecdir = "/home/hurou/Dropbox/haskell/microc/.stack-work/install/x86_64-linux/lts-9.11/8.0.2/libexec"
sysconfdir = "/home/hurou/Dropbox/haskell/microc/.stack-work/install/x86_64-linux/lts-9.11/8.0.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "microc_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "microc_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "microc_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "microc_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "microc_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "microc_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
