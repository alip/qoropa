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

module Qoropa.Buffer.Folder
    ( Attributes(..), Theme(..), LineData(..), StatusBar(..), StatusMessage(..), Folder(..)
    , emptyFolder
    , paint, load, new
    , scrollUp, scrollDown
    , selectPrev, selectNext
    , termSelected, cancelLoad
    ) where

import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, tryPutMVar, tryTakeMVar)
import Control.Monad           (when, unless)
import Data.IORef              (IORef, readIORef, writeIORef)
import Data.Maybe              (isJust, fromJust)

import Graphics.Vty
    ( Attr, Image, Picture
    , string, vert_cat
    , pic_for_image
    )

import qualified Email.Notmuch as NM
    ( Database, DatabaseOpenMode(..), databaseOpen, databaseClose
    , queryCreate, queryCountMessages, queryDestroy
    )

import Qoropa.Lock (Lock)
import qualified Qoropa.Lock as Lock (with)

import Qoropa.Widget.List
    ( Line(..), List(..)
    , emptyList
    , listLength, listIndex, listAppend
    , listRender
    , listScrollUp, listScrollDown
    , listSelectPrev, listSelectNext
    , toRegion
    )

import {-# SOURCE #-} Qoropa.UI (UIEvent(..))

data Attributes = Attributes
    { attrStatusBar     :: Attr
    , attrStatusMessage :: Attr
    , attrFill          :: Attr
    , attrName          :: (Attr, Attr)
    , attrTerm          :: (Attr, Attr)
    , attrCount         :: (Attr, Attr)
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
    , themeFormatLoading      :: (String, String) -> IO String
    , themeFormatLoadingDone  :: (String, String) -> IO String
    }

data LineData = LineData
    { folderName    :: String
    , folderTerm    :: String
    , folderCount   :: Integer
    }

data StatusBar = StatusBar
    { sBarCurrent :: Int
    , sBarTotal   :: Int
    }

data StatusMessage = StatusMessage { sMessage :: String }

data Folder = Folder
    { bufferList          :: List LineData
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

emptyFolder :: Theme -> IO Folder
emptyFolder theme = do
    cancelFolder <- newEmptyMVar
    return Folder
        { bufferList          = emptyList { listLineFill = fill }
        , bufferStatusBar     = emptyStatusBar
        , bufferStatusMessage = emptyStatusMessage
        , bufferTheme         = theme
        , bufferCancel        = cancelFolder
        }
    where
        fill = if isJust (themeFill theme)
            then Just (string (attrFill $ themeAttrs theme) (fromJust $ themeFill theme))
            else Nothing

paint :: Folder -> Int -> Picture
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

scrollUp :: (IORef Folder, Lock) -> Int -> IO ()
scrollUp (ref, lock) cnt = Lock.with lock $ scrollUp' ref cnt

scrollUp' :: IORef Folder -> Int -> IO ()
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
scrollDown :: (IORef Folder, Lock) -> Int -> Int -> IO ()
scrollDown (ref, lock) cols cnt = Lock.with lock $ scrollDown' ref cols cnt

scrollDown' :: IORef Folder -> Int -> Int -> IO ()
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

selectPrev :: (IORef Folder, Lock) -> Int -> IO ()
selectPrev (ref, lock) cnt = Lock.with lock $ selectPrev' ref cnt

selectPrev' :: IORef Folder -> Int -> IO ()
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
selectNext :: (IORef Folder, Lock) -> Int -> Int -> IO ()
selectNext (ref, lock) cols cnt = Lock.with lock $ selectNext' ref cols cnt

selectNext' :: IORef Folder -> Int -> Int -> IO ()
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

termSelected :: IORef Folder -> IO String
termSelected ref = do
    buf <- readIORef ref
    let ls   = bufferList buf
        line = listIndex ls (listSelected ls)
    return $ folderTerm $ lineData line

cancelLoad :: IORef Folder -> IO ()
cancelLoad ref = do
    buf <- readIORef ref
    tryPutMVar (bufferCancel buf) ()
    return ()

isCancelledLoad :: IORef Folder -> IO Bool
isCancelledLoad ref = do
    buf    <- readIORef ref
    status <- tryTakeMVar (bufferCancel buf)
    return $ isJust status

loadOne :: IORef Folder -> (String, String) -> Integer -> IO ()
loadOne ref (name, term) cnt = do
    buf <- readIORef ref

    let theme = bufferTheme buf
        len   = listLength $ bufferList buf
        ld    = LineData { folderName  = name
                         , folderTerm  = term
                         , folderCount = cnt
                         }
        line  = Line { lineData   = ld
                     , lineRender = (themeDrawLine theme) (themeAttrs theme)
                     }

    writeIORef ref buf { bufferList      = listAppend (bufferList buf) line
                       , bufferStatusBar = (bufferStatusBar buf) { sBarTotal = len + 1 }
                       }

load :: (IORef Folder, Lock) -> MVar UIEvent -> NM.Database -> [(String, String)] -> IO ()
load _ _ _ [] = return ()
load (ref, lock) mvar db ((name, term):xs) = do
    Lock.with lock $ do
        buf <- readIORef ref
        msg <- themeFormatLoading (bufferTheme buf) (name, term)
        writeIORef ref buf { bufferStatusMessage = (bufferStatusMessage buf) { sMessage = msg }
                           }
        putMVar mvar Redraw

    (Just query) <- NM.queryCreate db term
    cnt <- NM.queryCountMessages query
    NM.queryDestroy query

    when (cnt > 0) $
        Lock.with lock $ do
            loadOne ref (name, term) cnt

            buf <- readIORef ref
            msg <- themeFormatLoadingDone (bufferTheme buf) (name, term)
            writeIORef ref buf { bufferStatusMessage = (bufferStatusMessage buf) { sMessage = msg }
                               }

            putMVar mvar Redraw

    cancelled <- isCancelledLoad ref
    unless cancelled $ load (ref, lock) mvar db xs

new :: (IORef Folder, Lock) -> MVar UIEvent -> FilePath -> [(String, String)] -> IO ()
new (ref, lock) mvar fp folders = do
    (Just db) <- NM.databaseOpen fp NM.ModeReadOnly
    load (ref, lock) mvar db folders
    NM.databaseClose db

-- vim: set ft=haskell et ts=4 sts=4 sw=4 fdm=marker :
