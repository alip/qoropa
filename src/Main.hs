{-
- Qoropa -- Love Your Mail!
- Copyright © 2010 Ali Polatel
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

module Main (main) where

import Prelude  hiding          (catch)
import Control.Exception        (catch, SomeException(..))
import Control.Monad            (unless)
import System.Environment.UTF8  (getArgs, getProgName)
import System.Exit              (exitFailure)
import System.IO                (hPrint, hFlush, stderr)
import System.Info              (arch, os)
import System.Posix.Process     (executeFile)

import Qoropa.Util              (getQoropaDir, recompile)
import Qoropa.Config            (defaultConfig)
import Qoropa                   (qoropa)

main :: IO ()
main = do
    args <- getArgs

    case args of
        []              -> launch >> qoropa defaultConfig
        ["--help"]      -> usage
        ["--version"]   -> putStrLn "qoropa-0.1"
        ["--recompile"] -> recompile True >>= flip unless exitFailure
        _               -> fail "unrecognized flags"

usage :: IO ()
usage = do
    self <- getProgName
    putStr . unlines $
        [ concat ["Usage: ", self, " [OPTION]"]
        , "Options:"
        , "  --help           Display help"
        , "  --version        Display version"
        , "  --recompile      Recompile your ~/.qoropa/qoropa.hs"
        ]

launch :: IO ()
launch = do
    recompile False
    dir <- getQoropaDir
    args <- getArgs
    catch (executeFile (dir ++ "/qoropa-" ++ arch ++ "-" ++ os) False args Nothing)
          (\(SomeException e) -> hPrint stderr e >> hFlush stderr)
    return ()

-- vim: set ft=haskell et ts=4 sts=4 sw=4 fdm=marker :
