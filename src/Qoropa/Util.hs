{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}
{-
- Qoropa -- Love Your Mail!
- Copyright © 2010 Ali Polatel
- Based in part upon XMonad which is:
-   Copyright (c) 2007 Spencer Janssen
- Based in part upon gregorycollins/homepage which is:
-   Copyright (C) 2009 Gregory Collins
-
- This file is part of the Qoropa mail reader. Qoropa is free software;
- you can redistribute it and/or modify it under the terms of the GNU General
- Public License version 2, as published by the Free Software Foundation.
-
- Qoropa is distributed in the hope that it will be useful, but WITHOUT ANY
- WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
- A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
-
- You should have received a copy of the GNU General Public License along with
- this program; if not, write to the Free Software Foundation, Inc., 59 Temple
- Place, Suite 330, Boston, MA  02111-1307  USA
-
- Author: Ali Polatel <alip@exherbo.org>
-}

module Qoropa.Util
    ( beep
    , expandTilde
    , getQoropaDir
    , relativeTime
    , recompile
    ) where

import Prelude hiding        (catch)
import Control.Applicative   ((<$>))
import Control.Exception     (catch, bracket, SomeException(..))
import Control.Monad         (filterM)
import Data.List             ((\\))

import Data.Time
    ( TimeZone, UTCTime, NominalDiffTime
    , diffUTCTime, utcToLocalTime, formatTime
    , getCurrentTime, getCurrentTimeZone
    )
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Foreign.C.Types       (CTime)
import System.Locale         (defaultTimeLocale)

import System.Exit           (ExitCode(..))
import System.IO             (openFile, IOMode(..), hClose)
import System.Info           (arch, os)
import System.FilePath.Posix ((</>))
import System.Process        (runProcess, waitForProcess)
import System.Directory
    ( doesDirectoryExist
    , getDirectoryContents, getAppUserDataDirectory, getModificationTime
    , getHomeDirectory
    )

beep :: IO ()
beep = putChar '\a'

expandTilde :: FilePath -> IO FilePath
expandTilde ('~':'/':xs) = do
    home <- getHomeDirectory
    return $ home </> xs
expandTilde f = return f

getQoropaDir :: IO FilePath
getQoropaDir = getAppUserDataDirectory "qoropa"

humanReadableTimeDiff :: TimeZone -> UTCTime -> UTCTime -> String
humanReadableTimeDiff tz curTime oldTime =
    helper diff
    where
        diff = diffUTCTime curTime oldTime

        minutes :: NominalDiffTime -> Double
        minutes n = realToFrac $ n / 60

        hours :: NominalDiffTime -> Double
        hours   n = (minutes n) / 60

        days :: NominalDiffTime -> Double
        days    n = (hours n) / 24

        years :: NominalDiffTime -> Double
        years   n = (days n) / 365

        i2s :: RealFrac a => a -> String
        i2s !n = show m
            where
                m :: Int
                m = truncate n

        old = utcToLocalTime tz oldTime

        today         = formatTime defaultTimeLocale "Today %R" old
        yesterday     = formatTime defaultTimeLocale "Yest. %R" old
        dayOfWeek     = formatTime defaultTimeLocale "%a. %R" old
        thisYear      = formatTime defaultTimeLocale "%B %d" old
        previousYears = formatTime defaultTimeLocale "%F" old

        helper !d | minutes d < 2  = "One min. ago"
                  | minutes d < 60 = i2s (minutes d) ++ " mins. ago"
                  | hours d < 24   = today
                  | hours d < 48   = yesterday
                  | days d < 5     = dayOfWeek
                  | years d < 1    = thisYear
                  | otherwise      = previousYears

relativeTime :: CTime -> IO String
relativeTime t = do
    tz  <- getCurrentTimeZone
    now <- getCurrentTime
    return $ humanReadableTimeDiff tz now old
    where
        old = posixSecondsToUTCTime $ realToFrac t

recompile :: Bool -> IO Bool
recompile force = do
    dir <- getQoropaDir
    let name = "qoropa-" ++ arch ++ "-" ++ os
        bin  = dir </> name
        base = dir </> "qoropa"
        err  = base ++ ".errors"
        src  = base ++ ".hs"
        lib  = dir </> "lib"

    libTs <- mapM getModTime . filter isSource =<< allFiles lib
    srcT  <- getModTime src
    binT  <- getModTime bin

    if force || any (binT <) (srcT : libTs)
        then do
            status <- bracket (openFile err WriteMode) hClose $ \h ->
                waitForProcess =<< runProcess "ghc" [ "--make"
                                                    , "qoropa.hs"
                                                    , "-i"
                                                    , "-ilib"
                                                    , "-fforce-recomp"
                                                    , "-v0"
                                                    , "-o", name
                                                    ] (Just dir)
                                        Nothing Nothing Nothing (Just h)

            return (status == ExitSuccess)
        else return True

    where
        getModTime f = catch (Just <$> getModificationTime f) (\(SomeException _) -> return Nothing)
        isSource = flip elem [".hs", ".lhs", ".hsc"]
        allFiles t = do
            let prep = map (t</>) . filter (`notElem` [".", ".."])
            cs <- prep <$> catch (getDirectoryContents t) (\(SomeException _) -> return [])
            ds <- filterM doesDirectoryExist cs
            concat . ((cs \\ ds):) <$> mapM allFiles ds

-- vim: set ft=haskell et ts=4 sts=4 sw=4 fdm=marker :
