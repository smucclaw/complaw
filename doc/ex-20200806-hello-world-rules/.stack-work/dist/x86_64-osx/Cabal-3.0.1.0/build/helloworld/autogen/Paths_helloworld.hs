{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_helloworld (
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
version = Version [0,0,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/mengwong/src/smucclaw/complaw/doc/ex-20200806-hello-world-rules/.stack-work/install/x86_64-osx/326e36b477f98946111511bdd86f83087c82b45890fa1ce0f9ab25511b0454fb/8.8.3/bin"
libdir     = "/Users/mengwong/src/smucclaw/complaw/doc/ex-20200806-hello-world-rules/.stack-work/install/x86_64-osx/326e36b477f98946111511bdd86f83087c82b45890fa1ce0f9ab25511b0454fb/8.8.3/lib/x86_64-osx-ghc-8.8.3/helloworld-0.0.0.0-GCjuicc2poQ6gCYbmERVkQ-helloworld"
dynlibdir  = "/Users/mengwong/src/smucclaw/complaw/doc/ex-20200806-hello-world-rules/.stack-work/install/x86_64-osx/326e36b477f98946111511bdd86f83087c82b45890fa1ce0f9ab25511b0454fb/8.8.3/lib/x86_64-osx-ghc-8.8.3"
datadir    = "/Users/mengwong/src/smucclaw/complaw/doc/ex-20200806-hello-world-rules/.stack-work/install/x86_64-osx/326e36b477f98946111511bdd86f83087c82b45890fa1ce0f9ab25511b0454fb/8.8.3/share/x86_64-osx-ghc-8.8.3/helloworld-0.0.0.0"
libexecdir = "/Users/mengwong/src/smucclaw/complaw/doc/ex-20200806-hello-world-rules/.stack-work/install/x86_64-osx/326e36b477f98946111511bdd86f83087c82b45890fa1ce0f9ab25511b0454fb/8.8.3/libexec/x86_64-osx-ghc-8.8.3/helloworld-0.0.0.0"
sysconfdir = "/Users/mengwong/src/smucclaw/complaw/doc/ex-20200806-hello-world-rules/.stack-work/install/x86_64-osx/326e36b477f98946111511bdd86f83087c82b45890fa1ce0f9ab25511b0454fb/8.8.3/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "helloworld_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "helloworld_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "helloworld_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "helloworld_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "helloworld_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "helloworld_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
