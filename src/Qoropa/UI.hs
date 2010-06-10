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
    , start, exit, mainLoop
    , currentBuffer, redraw
    , scrollUp, scrollDown, selectPrev, selectNext
    ) where

import Control.Concurrent       (ThreadId, forkIO, myThreadId)
import Control.Concurrent.MVar  (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Exception        (throwTo)
import Control.Monad            (forever)
import Data.IORef               (IORef, newIORef, readIORef, writeIORef)
import System.Exit              (ExitCode(..))
import System.Posix.Signals     (raiseSignal, sigTSTP)

import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Map as Map

import Graphics.Vty
    ( Vty, mkVty, reserve_display, shutdown, terminal, update
    , DisplayRegion(..), display_bounds
    , Event(..), Key(..), Modifier(..), next_event
    )

import Qoropa.Lock (Lock)
import qualified Qoropa.Lock as Lock (new)

import qualified Qoropa.Buffer.Search as Search
    ( emptySearch, paint, new
    , scrollUp, scrollDown
    , selectNext, selectPrev
    )

import Qoropa.Buffer (Buffer(..))
import Qoropa.Config (QoropaConfig(..))
import Qoropa.Util   (expandTilde)

data UI = UI
    { vty        :: Vty
    , uiEvent    :: MVar UIEvent
    , uiThread   :: ThreadId
    , scrSize    :: IORef (Int, Int)
    , bufSeq     :: IORef (Seq (Buffer, Lock))
    , bufCurrent :: IORef Int
    }

data UIEvent = VtyEvent Event
               | NewSearch String
               | Redraw
               | Exit

currentBuffer :: UI -> IO (Buffer, Lock)
currentBuffer ui = do
    sq <- readIORef (bufSeq ui)
    cur <- readIORef (bufCurrent ui)
    return $ Seq.index sq (cur - 1)

redraw :: UI -> IO ()
redraw ui = do
    (buf, _) <- currentBuffer ui

    case buf of
        BufSearch ref -> do
            sbuf <- readIORef ref
            (cols, _) <- readIORef (scrSize ui)
            update (vty ui) $ Search.paint sbuf cols
        _ -> return ()

scrollUp :: Int -> UI -> IO ()
scrollUp count ui = do
    (buf, lock) <- currentBuffer ui
    case buf of
        BufSearch ref -> do
            forkIO $ Search.scrollUp (ref, lock) count >> putMVar (uiEvent ui) Redraw
            return ()
        _ -> return ()

scrollDown :: Int -> UI -> IO ()
scrollDown count ui = do
    (buf, lock) <- currentBuffer ui
    case buf of
        BufSearch ref -> do
            (cols, _) <- readIORef (scrSize ui)
            forkIO $ Search.scrollDown (ref, lock) cols count >> putMVar (uiEvent ui) Redraw
            return ()
        _ -> return ()

selectPrev :: Int -> UI -> IO ()
selectPrev count ui = do
    (buf, lock) <- currentBuffer ui
    case buf of
        BufSearch ref -> do
            forkIO $ Search.selectPrev (ref, lock) count >> putMVar (uiEvent ui) Redraw
            return ()
        _ -> return ()

selectNext :: Int -> UI -> IO ()
selectNext count ui = do
    (buf, lock) <- currentBuffer ui
    case buf of
        BufSearch ref -> do
            (cols, _) <- readIORef (scrSize ui)
            forkIO $ Search.selectNext (ref, lock) cols count >> putMVar (uiEvent ui) Redraw
            return ()
        _ -> return ()

start :: IO UI
start = do
    tid          <- myThreadId
    eventUI      <- newEmptyMVar

    vtyUI <- mkVty
    DisplayRegion x0 y0 <- display_bounds $ terminal vtyUI
    size <- newIORef (fromEnum y0, fromEnum x0)
    sq <- newIORef Seq.empty
    cur <- newIORef 0
    let ui = UI { vty        = vtyUI
                , scrSize    = size
                , uiEvent    = eventUI
                , uiThread   = tid
                , bufSeq     = sq
                , bufCurrent = cur
                }
        getcLoop = forever $ do
            event <- next_event vtyUI
            putMVar eventUI (VtyEvent event)

    forkIO getcLoop
    return ui

exit :: UI -> IO ()
exit ui = do
    reserve_display $ terminal $ vty ui
    shutdown $ vty ui
    throwTo (uiThread ui) ExitSuccess
    return ()

mainLoop :: QoropaConfig -> UI -> IO ()
mainLoop conf ui = do
    path <- expandTilde (databasePath conf)
    putMVar (uiEvent ui) $ NewSearch "tag:inbox"
    eventLoop path
    where
        eventLoop :: FilePath -> IO ()
        eventLoop path = forever $ do
            event <- takeMVar (uiEvent ui)
            case event of
                VtyEvent e ->
                    case e of
                        (EvResize x y) -> writeIORef (scrSize ui) (y, x) >> redraw ui
                        (EvKey (KASCII 'z') [MCtrl]) -> raiseSignal sigTSTP
                        _ ->
                            case Map.lookup e (keys conf) of
                                Just f -> f ui
                                Nothing -> return ()
                NewSearch term -> do
                    sq <- readIORef (bufSeq ui)
                    searchRef  <- newIORef (Search.emptySearch (themeSearch conf))
                    searchLock <- Lock.new
                    writeIORef (bufSeq ui) (sq Seq.|> (BufSearch searchRef, searchLock))
                    writeIORef (bufCurrent ui) (Seq.length sq + 1)
                    forkIO $ Search.new (searchRef, searchLock) (uiEvent ui) path term
                    return ()
                Redraw -> redraw ui
                Exit -> exit ui

-- vim: set ft=haskell et ts=4 sts=4 sw=4 fdm=marker :
