{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_tui (
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
version = Version [0,0,0,1] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/benjamin.macadam/Teaching/Haskell-Checkers-Frontend/.stack-work/install/x86_64-osx/9428a163867a229abd25dc18664619a549ab4d3666cd6172026da9742f14b26b/8.4.4/bin"
libdir     = "/Users/benjamin.macadam/Teaching/Haskell-Checkers-Frontend/.stack-work/install/x86_64-osx/9428a163867a229abd25dc18664619a549ab4d3666cd6172026da9742f14b26b/8.4.4/lib/x86_64-osx-ghc-8.4.4/tui-0.0.0.1-6DT8dSM2lNwDNAp5OgcVpJ"
dynlibdir  = "/Users/benjamin.macadam/Teaching/Haskell-Checkers-Frontend/.stack-work/install/x86_64-osx/9428a163867a229abd25dc18664619a549ab4d3666cd6172026da9742f14b26b/8.4.4/lib/x86_64-osx-ghc-8.4.4"
datadir    = "/Users/benjamin.macadam/Teaching/Haskell-Checkers-Frontend/.stack-work/install/x86_64-osx/9428a163867a229abd25dc18664619a549ab4d3666cd6172026da9742f14b26b/8.4.4/share/x86_64-osx-ghc-8.4.4/tui-0.0.0.1"
libexecdir = "/Users/benjamin.macadam/Teaching/Haskell-Checkers-Frontend/.stack-work/install/x86_64-osx/9428a163867a229abd25dc18664619a549ab4d3666cd6172026da9742f14b26b/8.4.4/libexec/x86_64-osx-ghc-8.4.4/tui-0.0.0.1"
sysconfdir = "/Users/benjamin.macadam/Teaching/Haskell-Checkers-Frontend/.stack-work/install/x86_64-osx/9428a163867a229abd25dc18664619a549ab4d3666cd6172026da9742f14b26b/8.4.4/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "tui_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "tui_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "tui_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "tui_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "tui_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "tui_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
