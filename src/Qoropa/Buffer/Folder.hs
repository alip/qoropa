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
    ( Attributes(..), Theme(..), Line(..), StatusBar(..), StatusMessage(..), Folder(..)
    , emptyFolder
    , paint, load, new
    , scrollUp, scrollDown
    , selectPrev, selectNext
    , termSelected, cancelLoad
    ) where

import Control.Concurrent.MVar  (MVar, newEmptyMVar, putMVar, tryPutMVar, tryTakeMVar)
import Control.Monad            (when, unless)
import Data.IORef               (IORef, readIORef, writeIORef)

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
    , themeFill               :: String
    , themeDrawLine           :: Attributes -> Int -> Line -> Image
    , themeDrawStatusBar      :: Attributes -> StatusBar -> Image
    , themeDrawStatusMessage  :: Attributes -> StatusMessage -> Image
    , themeFormatHitTheTop    :: IO String
    , themeFormatHitTheBottom :: IO String
    , themeFormatLoading      :: (String, String) -> IO String
    , themeFormatLoadingDone  :: (String, String) -> IO String
    }

data Line = Line
    { lineIndex     :: Int
    , folderName    :: String
    , folderTerm    :: String
    , folderCount   :: Integer
    }

data StatusBar = StatusBar
    { sBarCurrent :: Int
    , sBarTotal   :: Int
    }

data StatusMessage = StatusMessage { sMessage :: String }

data Folder = Folder
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

emptyFolder :: Theme -> IO Folder
emptyFolder theme = do
    cancelFolder <- newEmptyMVar
    return Folder
        { bufferFirst         = 1
        , bufferSelected      = 1
        , bufferLines         = []
        , bufferStatusBar     = emptyStatusBar
        , bufferStatusMessage = emptyStatusMessage
        , bufferTheme         = theme
        , bufferCancel        = cancelFolder
        }

paint :: Folder -> Int -> Picture
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

scrollUp :: (IORef Folder, Lock) -> Int -> IO ()
scrollUp (ref, lock) count = Lock.with lock $ scrollUp' ref count

scrollUp' :: IORef Folder -> Int -> IO ()
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

scrollDown :: (IORef Folder, Lock) -> Int -> Int -> IO ()
scrollDown (ref, lock) cols count = Lock.with lock $ scrollDown' ref cols count

scrollDown' :: IORef Folder -> Int -> Int -> IO ()
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

selectPrev :: (IORef Folder, Lock) -> Int -> IO ()
selectPrev (ref, lock) count = Lock.with lock $ selectPrev' ref count

selectPrev' :: IORef Folder -> Int -> IO ()
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

selectNext :: (IORef Folder, Lock) -> Int -> Int -> IO ()
selectNext (ref, lock) cols count = Lock.with lock $ selectNext' ref cols count

selectNext' :: IORef Folder -> Int -> Int -> IO ()
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

termSelected :: IORef Folder -> IO String
termSelected ref = do
    buf <- readIORef ref
    let line = (bufferLines buf) !! (bufferSelected buf - 1)
    return $ folderTerm line

cancelLoad :: IORef Folder -> IO ()
cancelLoad ref = do
    buf <- readIORef ref
    tryPutMVar (bufferCancel buf) ()
    return ()

isCancelledLoad :: IORef Folder -> IO Bool
isCancelledLoad ref = do
    buf    <- readIORef ref
    status <- tryTakeMVar (bufferCancel buf)
    case status of
        Just _  -> return True
        Nothing -> return False

loadOne :: IORef Folder -> (String, String) -> Integer -> IO ()
loadOne ref (name, term) count = do
    buf <- readIORef ref
    let len  = length $ bufferLines buf
        line = Line { lineIndex   = len + 1
                    , folderName  = name
                    , folderTerm  = term
                    , folderCount = count
                    }

    writeIORef ref buf { bufferLines = bufferLines buf ++ [line]
                       , bufferStatusBar = (bufferStatusBar buf) { sBarTotal = len + 1 }
                       }

load :: (IORef Folder, Lock) -> MVar UIEvent -> NM.Database -> [(String, String)] -> IO ()
load _ _ _ [] = return ()
load (ref, lock) mvar db ((name, term):xs) = do
    Lock.with lock $ do
        buf <- readIORef ref
        msg <- (themeFormatLoading (bufferTheme buf)) (name, term)
        writeIORef ref buf { bufferStatusMessage = (bufferStatusMessage buf) { sMessage = msg }
                           }
        putMVar mvar Redraw

    (Just query) <- NM.queryCreate db term
    count <- NM.queryCountMessages query
    NM.queryDestroy query

    when (count > 0) $
        Lock.with lock $ do
            loadOne ref (name, term) count

            buf <- readIORef ref
            msg <- (themeFormatLoadingDone (bufferTheme buf)) (name, term)
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
