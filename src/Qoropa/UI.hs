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
    , openSelected, cancelOperation
    , switchBuffer, switchBufferNext, switchBufferPrev
    ) where

import Control.Concurrent       (ThreadId, forkIO, myThreadId)
import Control.Concurrent.MVar  (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Exception        (throwTo)
import Control.Monad            (forever)
import Data.IORef               (IORef, newIORef, readIORef, writeIORef, modifyIORef)
import System.Exit              (ExitCode(..))
import System.Posix.Signals     (raiseSignal, sigTSTP)

import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Map as Map (lookup)

import System.Log.Logger
    ( updateGlobalLogger, rootLoggerName, setHandlers, setLevel
    , debugM, noticeM
    )
import System.Log.Handler.Simple (GenericHandler(..))

import Graphics.Vty
    ( Vty, mkVtyEscDelay, reserve_display, shutdown, terminal, update
    , DisplayRegion(..), display_bounds
    , Event(..), Key(..), Modifier(..), next_event
    )

import Qoropa.Lock (Lock)
import qualified Qoropa.Lock as Lock (new)

import qualified Qoropa.Buffer.Folder as Folder
    ( emptyFolder, paint, new, cancelLoad
    , scrollUp, scrollDown
    , selectNext, selectPrev
    , termSelected
    )

import qualified Qoropa.Buffer.Log as Log
    ( emptyLog, paint, handler
    , scrollUp, scrollDown
    , selectNext, selectPrev
    )

import qualified Qoropa.Buffer.Search as Search
    ( emptySearch, paint, new, cancelLoad
    , scrollUp, scrollDown
    , selectNext, selectPrev
    )

import Qoropa.Buffer (Buffer(..))
import Qoropa.Config (QoropaConfig(..))
import Qoropa.Util   (beep, expandTilde)

data UI = UI
    { userConfig :: QoropaConfig
    , vty        :: Vty
    , uiEvent    :: MVar UIEvent
    , uiThread   :: ThreadId
    , scrSize    :: IORef (Int, Int)
    , bufSeq     :: IORef (Seq (Buffer, Lock))
    , bufCurrent :: IORef Int
    }

data UIEvent = VtyEvent Event
               | NewFolder
               | NewSearch String
               | Redraw
               | Exit

currentBuffer :: UI -> IO (Buffer, Lock)
currentBuffer ui = do
    sq <- readIORef (bufSeq ui)
    cur <- readIORef (bufCurrent ui)
    return $ Seq.index sq cur

switchBuffer :: Int -> UI -> IO ()
switchBuffer to ui = do
    sq <- readIORef (bufSeq ui)

    if to < 0 || to >= Seq.length sq
        then beep
        else writeIORef (bufCurrent ui) to >> redraw ui

switchBufferNext :: UI -> IO ()
switchBufferNext ui = do
    cur <- readIORef (bufCurrent ui)
    switchBuffer (cur + 1) ui

switchBufferPrev :: UI -> IO ()
switchBufferPrev ui = do
    cur <- readIORef (bufCurrent ui)
    switchBuffer (cur - 1) ui

redraw :: UI -> IO ()
redraw ui = do
    (buf, _) <- currentBuffer ui
    (cols, _) <- readIORef (scrSize ui)

    case buf of
        BufFolder ref -> do
            rbuf <- readIORef ref
            update (vty ui) $ Folder.paint rbuf cols
        BufLog ref -> do
            rbuf <- readIORef ref
            update (vty ui) $ Log.paint rbuf cols
        BufSearch ref -> do
            rbuf <- readIORef ref
            update (vty ui) $ Search.paint rbuf cols
        _ -> return ()

scrollUp :: Int -> UI -> IO ()
scrollUp count ui = do
    (buf, lock) <- currentBuffer ui
    case buf of
        BufFolder ref -> do
            forkIO $ Folder.scrollUp (ref, lock) count >> putMVar (uiEvent ui) Redraw
            return ()
        BufLog ref -> do
            forkIO $ Log.scrollUp (ref, lock) count >> putMVar (uiEvent ui) Redraw
            return ()
        BufSearch ref -> do
            forkIO $ Search.scrollUp (ref, lock) count >> putMVar (uiEvent ui) Redraw
            return ()
        _ -> return ()

scrollDown :: Int -> UI -> IO ()
scrollDown count ui = do
    (buf, lock) <- currentBuffer ui
    (cols, _) <- readIORef (scrSize ui)
    case buf of
        BufFolder ref -> do
            forkIO $ Folder.scrollDown (ref, lock) cols count >> putMVar (uiEvent ui) Redraw
            return ()
        BufLog ref -> do
            forkIO $ Log.scrollDown (ref, lock) cols count >> putMVar (uiEvent ui) Redraw
            return ()
        BufSearch ref -> do
            forkIO $ Search.scrollDown (ref, lock) cols count >> putMVar (uiEvent ui) Redraw
            return ()
        _ -> return ()

selectPrev :: Int -> UI -> IO ()
selectPrev count ui = do
    (buf, lock) <- currentBuffer ui
    case buf of
        BufFolder ref -> do
            forkIO $ Folder.selectPrev (ref, lock) count >> putMVar (uiEvent ui) Redraw
            return ()
        BufLog ref -> do
            forkIO $ Log.selectPrev (ref, lock) count >> putMVar (uiEvent ui) Redraw
            return ()
        BufSearch ref -> do
            forkIO $ Search.selectPrev (ref, lock) count >> putMVar (uiEvent ui) Redraw
            return ()
        _ -> return ()

selectNext :: Int -> UI -> IO ()
selectNext count ui = do
    (buf, lock) <- currentBuffer ui
    (cols, _) <- readIORef (scrSize ui)
    case buf of
        BufFolder ref -> do
            forkIO $ Folder.selectNext (ref, lock) cols count >> putMVar (uiEvent ui) Redraw
            return ()
        BufLog ref -> do
            forkIO $ Log.selectNext (ref, lock) cols count >> putMVar (uiEvent ui) Redraw
            return ()
        BufSearch ref -> do
            forkIO $ Search.selectNext (ref, lock) cols count >> putMVar (uiEvent ui) Redraw
            return ()
        _ -> return ()

openSelected :: UI -> IO ()
openSelected ui = do
    (buf, _) <- currentBuffer ui
    case buf of
        BufFolder ref -> do
            forkIO $ do
                term <- Folder.termSelected ref
                putMVar (uiEvent ui) $ NewSearch term
            return ()
        _ -> beep

cancelOperation :: UI -> IO ()
cancelOperation ui = do
    (buf, _) <- currentBuffer ui
    case buf of
        BufFolder ref -> Folder.cancelLoad ref
        BufSearch ref -> Search.cancelLoad ref
        _ -> beep

start :: QoropaConfig -> IO UI
start cfg = do
    tid     <- myThreadId
    eventUI <- newEmptyMVar

    vtyUI <- mkVtyEscDelay (configVtyEscDelay cfg)
    DisplayRegion x0 y0 <- display_bounds $ terminal vtyUI
    size <- newIORef (fromEnum y0, fromEnum x0)
    sq <- newIORef Seq.empty
    cur <- newIORef (-1)
    let ui = UI { userConfig = cfg
                , vty        = vtyUI
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

mainLoop :: UI -> IO ()
mainLoop ui = do
    path <- expandTilde $ configDatabasePath $ userConfig ui

    -- Initialize the log buffer
    el <- Log.emptyLog $ configThemeLog $ userConfig ui
    logRef  <- newIORef el
    logLock <- Lock.new
    modifyIORef (bufSeq ui) (\sq -> sq Seq.|> (BufLog logRef, logLock))
    writeIORef (bufCurrent ui) 0

    -- Logging to stderr is bad mmkay?
    updateGlobalLogger rootLoggerName $ setHandlers ([] :: [GenericHandler ()])

    -- Add the log handler
    let logHandler = Log.handler (logRef, logLock) (uiEvent ui) (configLogPriority $ userConfig ui)
    updateGlobalLogger "Qoropa" $ setLevel (configLogPriority $ userConfig ui) . setHandlers [logHandler]
    noticeM "Qoropa" "Welcome to Qoropa!"

    forkIO $ putMVar (uiEvent ui) NewFolder
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
                            case Map.lookup e (configKeys $ userConfig ui) of
                                Just f -> f ui
                                Nothing -> debugM "Qoropa" $ "Unhandled event: " ++ show e
                NewFolder -> do
                    sq         <- readIORef (bufSeq ui)
                    ef         <- Folder.emptyFolder (configThemeFolder $ userConfig ui)
                    folderRef  <- newIORef ef
                    folderLock <- Lock.new
                    writeIORef (bufSeq ui) $ sq Seq.|> (BufFolder folderRef, folderLock)
                    writeIORef (bufCurrent ui) $ Seq.length sq
                    forkIO $ Folder.new (folderRef, folderLock) (uiEvent ui) path (configFolderList $ userConfig ui)
                    return ()
                NewSearch term -> do
                    sq         <- readIORef (bufSeq ui)
                    es         <- Search.emptySearch (configThemeSearch $ userConfig ui)
                    searchRef  <- newIORef es
                    searchLock <- Lock.new
                    writeIORef (bufSeq ui) $ sq Seq.|> (BufSearch searchRef, searchLock)
                    writeIORef (bufCurrent ui) $ Seq.length sq
                    forkIO $ Search.new (searchRef, searchLock) (uiEvent ui) path term
                    return ()
                Redraw -> redraw ui
                Exit -> exit ui

-- vim: set ft=haskell et ts=4 sts=4 sw=4 fdm=marker :
