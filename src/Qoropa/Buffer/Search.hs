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

module Qoropa.Buffer.Search
    ( Attributes(..), StatusBar(..), StatusMessage(..), Line(..), Theme(..), Search(..)
    , emptySearch
    , paint, new, cancelLoad
    , scrollUp, scrollDown
    , selectPrev, selectNext
    ) where

import Control.Concurrent.MVar  (MVar, newEmptyMVar, putMVar, tryTakeMVar, tryPutMVar)
import Control.Monad            (when, unless)
import Data.IORef               (IORef, readIORef, writeIORef)

import Codec.Binary.UTF8.String (decodeString)

import Graphics.Vty
    ( Attr, Image, Picture
    , string, vert_cat
    , pic_for_image
    )

import qualified Email.Notmuch as NM
    ( DatabaseOpenMode(..), databaseOpen, databaseClose
    , queryCreate, querySearchThreads
    , Threads, threadsValid, threadsGet, threadsMoveToNext
    , Thread, threadMatchedMessages, threadTotalMessages
    , threadAuthors, threadSubject, threadTags, threadDestroy
    , tagsDestroy
    )

import Qoropa.Notmuch (tagsToList)

import Qoropa.Lock (Lock)
import qualified Qoropa.Lock as Lock (with)

import {-# SOURCE #-} Qoropa.UI (UIEvent(..))

data Line = Line
    { lineIndex      :: Int
    , threadMatched  :: Integer
    , threadTotal    :: Integer
    , threadAuthors  :: String
    , threadSubject  :: String
    , threadTags     :: [String]
    }

data StatusBar = StatusBar
    { sBarTerm     :: String
    , sBarCurrent  :: Int
    , sBarTotal    :: Int
    }

data StatusMessage = StatusMessage
    { sMessage :: String
    }

data Attributes = Attributes
    { attrStatusBar     :: Attr
    , attrStatusMessage :: Attr
    , attrFill          :: Attr
    , attrCount         :: (Attr, Attr)
    , attrAuthor        :: (Attr, Attr)
    , attrSubject       :: (Attr, Attr)
    , attrTag           :: (Attr, Attr)
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
    , themeFormatLoading      :: String -> IO String
    , themeFormatLoadingDone  :: String -> IO String
    }

data Search = Search
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
    { sBarTerm    = ""
    , sBarCurrent = 1
    , sBarTotal   = 0
    }

emptyStatusMessage :: StatusMessage
emptyStatusMessage = StatusMessage { sMessage = " " }

emptySearch :: Theme -> IO Search
emptySearch theme = do
    cancelSearch <- newEmptyMVar
    return Search
        { bufferFirst         = 1
        , bufferSelected      = 1
        , bufferLines         = []
        , bufferStatusBar     = emptyStatusBar
        , bufferStatusMessage = emptyStatusMessage
        , bufferTheme         = theme
        , bufferCancel        = cancelSearch
        }

paint :: Search -> Int -> Picture
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

scrollUp :: (IORef Search, Lock) -> Int -> IO ()
scrollUp (ref, lock) count = Lock.with lock (scrollUp' ref count)

scrollUp' :: IORef Search -> Int -> IO ()
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

scrollDown :: (IORef Search, Lock) -> Int -> Int -> IO ()
scrollDown (ref, lock) cols count = Lock.with lock (scrollDown' ref cols count)

scrollDown' :: IORef Search -> Int -> Int -> IO ()
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

selectPrev :: (IORef Search, Lock) -> Int -> IO ()
selectPrev (ref, lock) count = Lock.with lock (selectPrev' ref count)

selectPrev' :: IORef Search -> Int -> IO ()
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

selectNext :: (IORef Search, Lock) -> Int -> Int -> IO ()
selectNext (ref, lock) cols count = Lock.with lock (selectNext' ref cols count)

selectNext' :: IORef Search -> Int -> Int -> IO ()
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

cancelLoad :: IORef Search -> IO ()
cancelLoad ref = do
    buf <- readIORef ref
    tryPutMVar (bufferCancel buf) ()
    return ()

isCancelledLoad :: IORef Search -> IO Bool
isCancelledLoad ref = do
    buf    <- readIORef ref
    status <- tryTakeMVar (bufferCancel buf)
    case status of
        Just _  -> return True
        Nothing -> return False

loadOne :: IORef Search -> NM.Thread -> IO ()
loadOne ref t = do
    buf <- readIORef ref
    let len = length $ bufferLines buf

    matched <- NM.threadMatchedMessages t
    total <- NM.threadTotalMessages t
    authors <- NM.threadAuthors t
    subject <- NM.threadSubject t

    tags <- NM.threadTags t
    taglist <- tagsToList tags
    NM.tagsDestroy tags

    let line = Line { lineIndex      = len + 1
                    , threadMatched  = matched
                    , threadTotal    = total
                    , threadAuthors  = decodeString authors
                    , threadSubject  = decodeString subject
                    , threadTags     = taglist
                    }

    writeIORef ref buf { bufferLines     = bufferLines buf ++ [line]
                       , bufferStatusBar = (bufferStatusBar buf) { sBarTotal = len + 1 }
                       }

load :: (IORef Search, Lock) -> MVar UIEvent -> NM.Threads -> IO ()
load (ref, lock) mvar ts = do
    v <- NM.threadsValid ts
    when v $ do
        (Just t) <- NM.threadsGet ts
        Lock.with lock (loadOne ref t >> putMVar mvar Redraw)
        NM.threadDestroy t >> NM.threadsMoveToNext ts
        cancelled <- isCancelledLoad ref
        unless cancelled $ load (ref, lock) mvar ts

new :: (IORef Search, Lock) -> MVar UIEvent -> FilePath -> String -> IO ()
new (ref, lock) mvar fp term = do
    Lock.with lock $ do
        buf <- readIORef ref
        msg <- (themeFormatLoading (bufferTheme buf)) term
        writeIORef ref buf { bufferStatusBar = (bufferStatusBar buf) { sBarTerm = term }
                           , bufferStatusMessage = (bufferStatusMessage buf) { sMessage = msg }
                           }
        putMVar mvar Redraw

    (Just db) <- NM.databaseOpen fp NM.ModeReadOnly
    (Just query) <- NM.queryCreate db term
    threads <- NM.querySearchThreads query
    load (ref, lock) mvar threads
    NM.databaseClose db

    Lock.with lock $ do
        buf <- readIORef ref
        msg <- (themeFormatLoadingDone (bufferTheme buf)) term
        writeIORef ref buf { bufferStatusMessage = (bufferStatusMessage buf) { sMessage = msg }
                           }
        putMVar mvar Redraw

-- vim: set ft=haskell et ts=4 sts=4 sw=4 fdm=marker :
