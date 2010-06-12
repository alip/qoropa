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

module Qoropa.Buffer.Log
    ( Attributes(..), Theme(..), Line(..), StatusBar(..), StatusMessage(..), Log(..)
    , emptyLog
    , paint, handler
    , scrollUp, scrollDown
    , selectPrev, selectNext
    ) where

import Control.Concurrent        (forkIO)
import Control.Concurrent.MVar   (MVar, newEmptyMVar, putMVar)
import Control.Monad             (when)
import Data.IORef                (IORef, readIORef, writeIORef)
import Data.Time                 (ZonedTime, getZonedTime)

import System.Log                (Priority, LogRecord)
import System.Log.Handler.Simple (GenericHandler(..))

import Graphics.Vty
    ( Attr, Image, Picture
    , string, vert_cat
    , pic_for_image
    )

import Qoropa.Lock  (Lock)
import qualified Qoropa.Lock as Lock (with)

import {-# SOURCE #-} Qoropa.UI (UIEvent(..))

data Attributes = Attributes
    { attrStatusBar     :: Attr
    , attrStatusMessage :: Attr
    , attrFill          :: Attr
    , attrTime          :: (Attr, Attr)
    , attrPriority      :: (Attr, Attr)
    , attrMessage       :: (Attr, Attr)
    , attrDefault       :: (Attr, Attr)
    }

data Theme = Theme
    { themeAttrs              :: Attributes
    , themeFill               :: String
    , themeDrawLine           :: Attributes -> Int -> Line -> Image
    , themeDrawStatusBar      :: Attributes -> StatusBar -> Image
    , themeDrawStatusMessage  :: Attributes -> StatusMessage -> Image
    , themeFormatHitTheTop    :: IO String
    , themeFormatHitTheBottom :: IO String
    }

data Line = Line
    { lineIndex   :: Int
    , logTime     :: ZonedTime
    , logRecord   :: LogRecord
    }

data StatusBar = StatusBar
    { sBarCurrent :: Int
    , sBarTotal   :: Int
    }

data StatusMessage = StatusMessage { sMessage :: String }

data Log = Log
    { bufferFirst         :: Int
    , bufferSelected      :: Int
    , bufferLines         :: [Line]
    , bufferStatusBar     :: StatusBar
    , bufferStatusMessage :: StatusMessage
    , bufferTheme         :: Theme
    , bufferCancel        :: MVar ()
    }

emptyStatusBar :: StatusBar
emptyStatusBar = StatusBar
    { sBarCurrent = 1
    , sBarTotal   = 0
    }

emptyStatusMessage :: StatusMessage
emptyStatusMessage = StatusMessage { sMessage = " " }

emptyLog :: Theme -> IO Log
emptyLog theme = do
    cancelLog <- newEmptyMVar
    return Log
        { bufferFirst         = 1
        , bufferSelected      = 1
        , bufferLines         = []
        , bufferStatusBar     = emptyStatusBar
        , bufferStatusMessage = emptyStatusMessage
        , bufferTheme         = theme
        , bufferCancel        = cancelLog
        }

paint :: Log -> Int -> Picture
paint buf height =
    pic_for_image $ vert_cat $ lns ++ fill ++ [bar, msg]
    where
        myFirst             = bufferFirst buf
        mySelected          = bufferSelected buf
        myLines             = bufferLines buf
        myTheme             = bufferTheme buf
        myAttr              = themeAttrs myTheme
        myDrawLine          = themeDrawLine myTheme
        myDrawStatusBar     = themeDrawStatusBar myTheme
        myDrawStatusMessage = themeDrawStatusMessage myTheme

        lns = take (height - 2) $ drop (myFirst - 1) $ map (myDrawLine myAttr mySelected) myLines

        len = length lns
        fill  = if len < height - 2
            then replicate (height - 2 - len) (string (attrFill myAttr) (themeFill myTheme))
            else []

        bar   = myDrawStatusBar myAttr (bufferStatusBar buf)
        msg   = myDrawStatusMessage myAttr (bufferStatusMessage buf)

scrollUp :: (IORef Log, Lock) -> Int -> IO ()
scrollUp (ref, lock) count = Lock.with lock $ scrollUp' ref count

scrollUp' :: IORef Log -> Int -> IO ()
scrollUp' ref count = do
    buf <- readIORef ref
    let first = bufferFirst buf - count
    let sel   = bufferSelected buf

    if first > 0
        then writeIORef ref buf { bufferFirst     = first
                                , bufferSelected  = sel - count + 1
                                , bufferStatusBar = (bufferStatusBar buf) { sBarCurrent = sel - count + 1 }
                                }
        else do
            msg <- themeFormatHitTheTop (bufferTheme buf)
            writeIORef ref buf { bufferStatusMessage = (bufferStatusMessage buf) { sMessage = msg }
                               }

scrollDown :: (IORef Log, Lock) -> Int -> Int -> IO ()
scrollDown (ref, lock) cols count = Lock.with lock $ scrollDown' ref cols count

scrollDown' :: IORef Log -> Int -> Int -> IO ()
scrollDown' ref cols count = do
    buf <- readIORef ref
    let len   = length $ bufferLines buf
    let first = bufferFirst buf + count
    let sel   = bufferSelected buf

    if first + cols - 3 <= len
        then writeIORef ref buf { bufferFirst = first
                                , bufferSelected = sel + count - 1
                                , bufferStatusBar = (bufferStatusBar buf) { sBarCurrent = sel + count - 1}
                                }
        else do
            msg <- themeFormatHitTheBottom (bufferTheme buf)
            writeIORef ref buf { bufferStatusMessage = (bufferStatusMessage buf) { sMessage = msg }
                               }

selectPrev :: (IORef Log, Lock) -> Int -> IO ()
selectPrev (ref, lock) count = Lock.with lock $ selectPrev' ref count

selectPrev' :: IORef Log -> Int -> IO ()
selectPrev' ref count = do
    buf <- readIORef ref
    let first = bufferFirst buf
    let sel   = bufferSelected buf

    if sel - count >= 1
        then do
            writeIORef ref buf { bufferSelected = sel - count
                               , bufferStatusBar = (bufferStatusBar buf) { sBarCurrent = sel - count }
                               }
            when (sel - count < first) $ scrollUp' ref count
        else do
            msg <- themeFormatHitTheTop (bufferTheme buf)
            writeIORef ref buf { bufferStatusMessage = (bufferStatusMessage buf) { sMessage = msg }
                               }

selectNext :: (IORef Log, Lock) -> Int -> Int -> IO ()
selectNext (ref, lock) cols count = Lock.with lock $ selectNext' ref cols count

selectNext' :: IORef Log -> Int -> Int -> IO ()
selectNext' ref cols count = do
    buf <- readIORef ref
    let sel = bufferSelected buf
    let len = length $ bufferLines buf
    let lst = bufferFirst buf + cols - 3

    if sel + count <= len
        then do
            writeIORef ref buf { bufferSelected  = sel + count
                               , bufferStatusBar = (bufferStatusBar buf) { sBarCurrent = sel + count }
                               }
            when (sel + count > lst) $ scrollDown' ref cols count
        else do
            msg <- themeFormatHitTheBottom (bufferTheme buf)
            writeIORef ref buf { bufferStatusMessage = (bufferStatusMessage buf) { sMessage = msg }
                               }

handler :: (IORef Log, Lock) -> MVar UIEvent -> Priority -> GenericHandler ()
handler (ref, lock) mvar pri =
    GenericHandler { priority  = pri
                   , privData  = ()
                   , writeFunc = myWriteFunc
                   , closeFunc = \_ -> return ()
                   }
    where
        myWriteFunc :: () -> LogRecord -> String -> IO ()
        myWriteFunc _ record _ =
            Lock.with lock $ do
                now <- getZonedTime
                buf <- readIORef ref
                let len  = length $ bufferLines buf
                    line = Line { lineIndex  = len + 1
                                , logTime    = now
                                , logRecord  = record
                                }

                writeIORef ref buf { bufferLines = bufferLines buf ++ [line]
                                   , bufferStatusBar = (bufferStatusBar buf) { sBarTotal = len + 1}
                                   }
                forkIO $ putMVar mvar Redraw
                return ()

-- vim: set ft=haskell et ts=4 sts=4 sw=4 fdm=marker :
