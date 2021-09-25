{-# LANGUAGE CPP                #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-missing-safe-haskell-mode #-}
module Paths_uniswap (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception  as Exception
import           Data.Version       (Version (..))
import           Prelude
import           System.Environment (getEnv)

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

bindir     = "/Users/michaljankun/.cabal/bin"
libdir     = "/Users/michaljankun/.cabal/lib/x86_64-osx-ghc-8.10.4.20210212/uniswap-0.1.0.0-inplace"
dynlibdir  = "/Users/michaljankun/.cabal/lib/x86_64-osx-ghc-8.10.4.20210212"
datadir    = "/Users/michaljankun/.cabal/share/x86_64-osx-ghc-8.10.4.20210212/uniswap-0.1.0.0"
libexecdir = "/Users/michaljankun/.cabal/libexec/x86_64-osx-ghc-8.10.4.20210212/uniswap-0.1.0.0"
sysconfdir = "/Users/michaljankun/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "uniswap_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "uniswap_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "uniswap_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "uniswap_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "uniswap_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "uniswap_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
