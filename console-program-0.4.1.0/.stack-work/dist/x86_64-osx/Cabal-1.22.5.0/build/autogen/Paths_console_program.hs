module Paths_console_program (
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
version = Version [0,4,1,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/mihaigiurgeanu/Projects/cold-widow/.stack-work/install/x86_64-osx/lts-6.25/7.10.3/bin"
libdir     = "/Users/mihaigiurgeanu/Projects/cold-widow/.stack-work/install/x86_64-osx/lts-6.25/7.10.3/lib/x86_64-osx-ghc-7.10.3/console-program-0.4.1.0-IyAwL61Q1p4FdscxhLOf04"
datadir    = "/Users/mihaigiurgeanu/Projects/cold-widow/.stack-work/install/x86_64-osx/lts-6.25/7.10.3/share/x86_64-osx-ghc-7.10.3/console-program-0.4.1.0"
libexecdir = "/Users/mihaigiurgeanu/Projects/cold-widow/.stack-work/install/x86_64-osx/lts-6.25/7.10.3/libexec"
sysconfdir = "/Users/mihaigiurgeanu/Projects/cold-widow/.stack-work/install/x86_64-osx/lts-6.25/7.10.3/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "console_program_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "console_program_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "console_program_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "console_program_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "console_program_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
