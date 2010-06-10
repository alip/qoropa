{-
- Qoropa -- Love Your Mail!
- Copyright © 2010 Ali Polatel
- Based in part upon Yi which is:
-   Copyright (C) 2007-8 JP Bernardy
-   Copyright (C) 2004-5 Don Stewart - http://www.cse.unsw.edu.au/~dons
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

module Qoropa.UI
    ( UI(..), UIEvent(..)
    , start, exit, mainLoop, redraw
    ) where

import Control.Concurrent       (ThreadId, forkIO, myThreadId)
import Control.Concurrent.MVar  (MVar, newEmptyMVar, putMVar, takeMVar, tryPutMVar, tryTakeMVar)
import Control.Exception        (throwTo)
import Control.Monad            (forever, when)
import Data.IORef               (IORef, newIORef, readIORef, writeIORef)
import System.Exit              (ExitCode(..))
import System.Posix.Signals     (raiseSignal, sigTSTP)
import Debug.Trace              (putTraceMsg)

import Graphics.Vty
    ( Vty, mkVty, reserve_display, shutdown, terminal, update
    , DisplayRegion(..), display_bounds
    , Event(..), Key(..), Modifier(..), next_event
    )

import qualified Qoropa.Lock as Lock (Lock, new)
import Qoropa.Search
    ( SearchBar(..), SearchMsg(..), SearchWindow(..)
    , emptySearchWindow, paintSearchWindow
    )
import qualified Qoropa.Search as S

data UI = UI
    { vty           :: Vty
    , uiEvent       :: MVar UIEvent
    , uiThread      :: ThreadId
    , scrSize       :: IORef (Int, Int)
    , searchWin     :: IORef SearchWindow
    , searchWinLock :: Lock.Lock
    }

data UIEvent = VtyEvent Event
               | RedrawSearch
               | Exit

redraw :: UI -> IO ()
redraw ui = do
    (cols, _) <- readIORef (scrSize ui)
    win <- readIORef (searchWin ui)
    update (vty ui) $ paintSearchWindow win cols

start :: IO UI
start = do
    tid          <- myThreadId
    eventUI      <- newEmptyMVar
    searchLockUI <- Lock.new

    vty <- mkVty
    DisplayRegion x0 y0 <- display_bounds $ terminal vty
    size <- newIORef (fromEnum y0, fromEnum x0)
    searchWindow <- newIORef emptySearchWindow
    let ui = UI { vty = vty
                , scrSize       = size
                , uiEvent       = eventUI
                , uiThread      = tid
                , searchWin     = searchWindow
                , searchWinLock = searchLockUI
                }
        getcLoop = forever $ do
            event <- next_event vty
            putMVar eventUI (VtyEvent event)

    forkIO getcLoop
    return ui

exit :: UI -> IO ()
exit ui = do
    reserve_display $ terminal $ vty ui
    shutdown $ vty ui
    throwTo (uiThread ui) ExitSuccess
    return ()

mainLoop :: UI -> IO ()
mainLoop ui = do
    let
        eventLoop :: IO ()
        eventLoop = forever $ do
            event <- takeMVar (uiEvent ui)
            case event of
                VtyEvent e -> do
                    case e of
                        (EvResize x y) -> writeIORef (scrSize ui) (y, x) >> redraw ui
                        (EvKey (KASCII 'l') [MCtrl]) -> redraw ui
                        (EvKey (KASCII 'z') [MCtrl]) -> raiseSignal sigTSTP
                        (EvKey (KASCII 'q') []) -> exit ui
                        (EvKey (KASCII 'j') []) -> forkIO (S.selectNext ui 1) >> return ()
                        (EvKey (KASCII 'k') []) -> forkIO (S.selectPrev ui 1) >> return ()
                        _ -> return ()
                RedrawSearch -> redraw ui
                Exit -> exit ui

    putMVar (uiEvent ui) RedrawSearch
    eventLoop

-- vim: set ft=haskell et ts=4 sts=4 sw=4 fdm=marker :
