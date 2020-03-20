{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_Mangrel (
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
version = Version [0,1,0,1] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/willis/Documents/557/Mangrel/.stack-work/install/x86_64-linux-tinfo6/c575562534f44701ec7c1c264c42fb5a0bff555089c133c815f47dfef3cde947/8.8.2/bin"
libdir     = "/home/willis/Documents/557/Mangrel/.stack-work/install/x86_64-linux-tinfo6/c575562534f44701ec7c1c264c42fb5a0bff555089c133c815f47dfef3cde947/8.8.2/lib/x86_64-linux-ghc-8.8.2/Mangrel-0.1.0.1-Cqi8fNPpQ22Igvc34EH2fg-Mangrel"
dynlibdir  = "/home/willis/Documents/557/Mangrel/.stack-work/install/x86_64-linux-tinfo6/c575562534f44701ec7c1c264c42fb5a0bff555089c133c815f47dfef3cde947/8.8.2/lib/x86_64-linux-ghc-8.8.2"
datadir    = "/home/willis/Documents/557/Mangrel/.stack-work/install/x86_64-linux-tinfo6/c575562534f44701ec7c1c264c42fb5a0bff555089c133c815f47dfef3cde947/8.8.2/share/x86_64-linux-ghc-8.8.2/Mangrel-0.1.0.1"
libexecdir = "/home/willis/Documents/557/Mangrel/.stack-work/install/x86_64-linux-tinfo6/c575562534f44701ec7c1c264c42fb5a0bff555089c133c815f47dfef3cde947/8.8.2/libexec/x86_64-linux-ghc-8.8.2/Mangrel-0.1.0.1"
sysconfdir = "/home/willis/Documents/557/Mangrel/.stack-work/install/x86_64-linux-tinfo6/c575562534f44701ec7c1c264c42fb5a0bff555089c133c815f47dfef3cde947/8.8.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Mangrel_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Mangrel_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "Mangrel_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "Mangrel_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Mangrel_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Mangrel_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
