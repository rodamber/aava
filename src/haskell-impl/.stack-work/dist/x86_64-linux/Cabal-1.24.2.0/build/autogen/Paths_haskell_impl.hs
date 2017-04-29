{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_haskell_impl (
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

bindir     = "/vagrant/src/haskell-impl/.stack-work/install/x86_64-linux/lts-8.12/8.0.2/bin"
libdir     = "/vagrant/src/haskell-impl/.stack-work/install/x86_64-linux/lts-8.12/8.0.2/lib/x86_64-linux-ghc-8.0.2/haskell-impl-0.1.0.0-5QZQUPOQJel4VPmddVin6V"
dynlibdir  = "/vagrant/src/haskell-impl/.stack-work/install/x86_64-linux/lts-8.12/8.0.2/lib/x86_64-linux-ghc-8.0.2"
datadir    = "/vagrant/src/haskell-impl/.stack-work/install/x86_64-linux/lts-8.12/8.0.2/share/x86_64-linux-ghc-8.0.2/haskell-impl-0.1.0.0"
libexecdir = "/vagrant/src/haskell-impl/.stack-work/install/x86_64-linux/lts-8.12/8.0.2/libexec"
sysconfdir = "/vagrant/src/haskell-impl/.stack-work/install/x86_64-linux/lts-8.12/8.0.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "haskell_impl_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "haskell_impl_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "haskell_impl_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "haskell_impl_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "haskell_impl_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "haskell_impl_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)