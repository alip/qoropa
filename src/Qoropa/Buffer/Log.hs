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
    ( Attributes(..), Theme(..), LineData(..), StatusBar(..), StatusMessage(..), Log(..)
    , emptyLog
    , paint, handler
    , scrollUp, scrollDown
    , selectPrev, selectNext
    ) where

import Control.Concurrent      (forkIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar)
import Data.IORef              (IORef, readIORef, writeIORef)
import Data.Maybe              (isJust, fromJust)
import Data.Time               (ZonedTime, getZonedTime)

import System.Log                (Priority, LogRecord)
import System.Log.Handler.Simple (GenericHandler(..))

import Graphics.Vty
    ( Attr, Image, Picture
    , string, vert_cat, pic_for_image
    )

import Qoropa.Lock  (Lock)
import qualified Qoropa.Lock as Lock (with)

import Qoropa.Widget.List
    ( Line(..), List(..)
    , emptyList
    , listLength, listAppend, listRender
    , listScrollUp, listScrollDown
    , listSelectPrev, listSelectNext
    , toRegion
    )

import {-# SOURCE #-} Qoropa.UI (UIEvent(..))

data LineData = LineData
    { lineDataTime   :: ZonedTime
    , lineDataRecord :: LogRecord
    }

data StatusBar = StatusBar
    { sBarCurrent :: Int
    , sBarTotal   :: Int
    }

data StatusMessage = StatusMessage { sMessage :: String }

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
    , themeFill               :: Maybe String
    , themeDrawLine           :: Attributes -> LineData -> Bool -> Image
    , themeDrawStatusBar      :: Attributes -> StatusBar -> Image
    , themeDrawStatusMessage  :: Attributes -> StatusMessage -> Image
    , themeFormatHitTheTop    :: IO String
    , themeFormatHitTheBottom :: IO String
    }

data Log = Log
    { bufferList          :: List LineData
    , bufferStatusBar     :: StatusBar
    , bufferStatusMessage :: StatusMessage
    , bufferTheme         :: Theme
    , bufferCancel        :: MVar ()
    }

emptyStatusBar :: StatusBar
emptyStatusBar = StatusBar
    { sBarCurrent = 0
    , sBarTotal   = 0
    }

emptyStatusMessage :: StatusMessage
emptyStatusMessage = StatusMessage { sMessage = " " }

emptyLog :: Theme -> IO Log
emptyLog theme = do
    cancelLog <- newEmptyMVar
    return Log
        { bufferList          = emptyList { listLineFill = fill }
        , bufferStatusBar     = emptyStatusBar
        , bufferStatusMessage = emptyStatusMessage
        , bufferTheme         = theme
        , bufferCancel        = cancelLog
        }
    where
        fill = if isJust (themeFill theme)
            then Just (string (attrFill $ themeAttrs theme) (fromJust $ themeFill theme))
            else Nothing

paint :: Log -> Int -> Picture
paint buf height =
    pic_for_image $ vert_cat $ lns ++ [bar, msg]
    where
        myTheme             = bufferTheme buf
        myAttr              = themeAttrs myTheme
        myDrawStatusBar     = themeDrawStatusBar myTheme
        myDrawStatusMessage = themeDrawStatusMessage myTheme
        lns = listRender (bufferList buf) (toRegion (height - 2) 0)
        bar = myDrawStatusBar myAttr (bufferStatusBar buf)
        msg = myDrawStatusMessage myAttr (bufferStatusMessage buf)

scrollUp :: (IORef Log, Lock) -> Int -> IO ()
scrollUp (ref, lock) cnt = Lock.with lock $ scrollUp' ref cnt

scrollUp' :: IORef Log -> Int -> IO ()
scrollUp' ref cnt = do
    buf <- readIORef ref

    case listScrollUp (bufferList buf) cnt  of
        Just ls -> writeIORef ref buf { bufferList      = ls
                                      , bufferStatusBar = (bufferStatusBar buf) { sBarCurrent = listSelected ls }
                                      }
        Nothing -> do
            msg <- themeFormatHitTheTop (bufferTheme buf)
            writeIORef ref buf { bufferStatusMessage = (bufferStatusMessage buf) { sMessage = msg }
                               }

scrollDown :: (IORef Log, Lock) -> Int -> Int -> IO ()
scrollDown (ref, lock) cols cnt = Lock.with lock $ scrollDown' ref cols cnt

scrollDown' :: IORef Log -> Int -> Int -> IO ()
scrollDown' ref cols cnt = do
    buf <- readIORef ref

    case listScrollDown (bufferList buf) (toRegion cols 0) cnt of
        Just ls -> writeIORef ref buf { bufferList      = ls
                                      , bufferStatusBar = (bufferStatusBar buf) { sBarCurrent = listSelected ls }
                                      }
        Nothing -> do
            msg <- themeFormatHitTheBottom (bufferTheme buf)
            writeIORef ref buf { bufferStatusMessage = (bufferStatusMessage buf) { sMessage = msg }
                               }

selectPrev :: (IORef Log, Lock) -> Int -> IO ()
selectPrev (ref, lock) cnt = Lock.with lock $ selectPrev' ref cnt

selectPrev' :: IORef Log -> Int -> IO ()
selectPrev' ref cnt = do
    buf <- readIORef ref

    case listSelectPrev (bufferList buf) cnt of
        Just ls -> writeIORef ref buf { bufferList      = ls
                                      , bufferStatusBar = (bufferStatusBar buf) { sBarCurrent = listSelected ls }
                                      }
        Nothing -> do
            msg <- themeFormatHitTheTop (bufferTheme buf)
            writeIORef ref buf { bufferStatusMessage = (bufferStatusMessage buf) { sMessage = msg }
                               }

selectNext :: (IORef Log, Lock) -> Int -> Int -> IO ()
selectNext (ref, lock) cols cnt = Lock.with lock $ selectNext' ref cols cnt

selectNext' :: IORef Log -> Int -> Int -> IO ()
selectNext' ref cols cnt = do
    buf <- readIORef ref

    case listSelectNext (bufferList buf) (toRegion cols 0) cnt of
        Just ls -> writeIORef ref buf { bufferList      = ls
                                      , bufferStatusBar = (bufferStatusBar buf) { sBarCurrent = listSelected ls }
                                      }
        Nothing -> do
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
        myWriteFunc _ record _ = do
            now <- getZonedTime
            Lock.with lock $ do
                buf <- readIORef ref
                let len   = listLength $ bufferList buf
                    theme = bufferTheme buf
                    line  = Line { lineData   = LineData { lineDataTime = now, lineDataRecord = record }
                                 , lineRender = (themeDrawLine theme) (themeAttrs theme)
                                 }
                writeIORef ref buf { bufferList = listAppend (bufferList buf) line
                                   , bufferStatusBar = (bufferStatusBar buf) { sBarTotal = len + 1}
                                   }
                forkIO $ putMVar mvar Redraw
                return ()

-- vim: set ft=haskell et ts=4 sts=4 sw=4 fdm=marker :
