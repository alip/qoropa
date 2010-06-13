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
    ( Attributes(..), Theme(..), StatusBar(..), StatusMessage(..), LineData(..), Search(..)
    , emptySearch
    , paint, new, cancelLoad
    , scrollUp, scrollDown
    , selectPrev, selectNext
    ) where

import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, tryTakeMVar, tryPutMVar)
import Control.Monad           (when, unless)
import Data.IORef              (IORef, readIORef, writeIORef)
import Data.Maybe              (isJust, fromJust)
import Foreign.C.Types         (CTime)

import System.Log.Logger       (rootLoggerName, debugM)

import Codec.Binary.UTF8.String (decodeString)

import Graphics.Vty
    ( Attr, Image, Picture
    , string, vert_cat, pic_for_image
    )

import qualified Email.Notmuch as NM
    ( DatabaseOpenMode(..)
    , databaseOpen, databaseClose
    , queryCreate, querySearchThreads
    , Threads, threadsValid, threadsGet, threadsMoveToNext
    , Thread, threadMatchedMessages, threadTotalMessages
    , threadAuthors, threadSubject, threadTags, threadDestroy
    , threadOldestDate, threadNewestDate
    , tagsDestroy
    )

import Qoropa.Notmuch (tagsToList)
import Qoropa.Util    (relativeTime)

import Qoropa.Lock (Lock)
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
    { threadOldestDate :: (CTime, String)
    , threadNewestDate :: (CTime, String)
    , threadMatched    :: Integer
    , threadTotal      :: Integer
    , threadAuthors    :: String
    , threadSubject    :: String
    , threadTags       :: [String]
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
    { attrStatusBar        :: Attr
    , attrStatusMessage    :: Attr
    , attrFill             :: Attr
    , attrTime             :: (Attr, Attr)
    , attrCount            :: (Attr, Attr)
    , attrAuthorMatched    :: (Attr, Attr)
    , attrAuthorNonMatched :: (Attr, Attr)
    , attrSubject          :: (Attr, Attr)
    , attrTag              :: (Attr, Attr)
    , attrDefault          :: (Attr, Attr)
    }

data Theme = Theme
    { themeAttrs              :: Attributes
    , themeFill               :: Maybe String
    , themeDrawLine           :: Attributes -> LineData -> Bool -> Image
    , themeDrawStatusBar      :: Attributes -> StatusBar -> Image
    , themeDrawStatusMessage  :: Attributes -> StatusMessage -> Image
    , themeFormatHitTheTop    :: IO String
    , themeFormatHitTheBottom :: IO String
    , themeFormatLoading      :: String -> IO String
    , themeFormatLoadingDone  :: String -> IO String
    }

data Search = Search
    { bufferList          :: List LineData
    , bufferStatusBar     :: StatusBar
    , bufferStatusMessage :: StatusMessage
    , bufferTheme         :: Theme
    , bufferCancel        :: MVar ()
    }

emptyStatusBar :: StatusBar
emptyStatusBar = StatusBar
    { sBarTerm    = ""
    , sBarCurrent = 0
    , sBarTotal   = 0
    }

emptyStatusMessage :: StatusMessage
emptyStatusMessage = StatusMessage { sMessage = " " }

emptySearch :: Theme -> IO Search
emptySearch theme = do
    cancelSearch <- newEmptyMVar
    return Search
        { bufferList          = emptyList { listLineFill = fill }
        , bufferStatusBar     = emptyStatusBar
        , bufferStatusMessage = emptyStatusMessage
        , bufferTheme         = theme
        , bufferCancel        = cancelSearch
        }
    where
        fill = if isJust (themeFill theme)
            then Just (string (attrFill $ themeAttrs theme) (fromJust $ themeFill theme))
            else Nothing

paint :: Search -> Int -> Picture
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

scrollUp :: (IORef Search, Lock) -> Int -> IO ()
scrollUp (ref, lock) cnt = Lock.with lock $ scrollUp' ref cnt

scrollUp' :: IORef Search -> Int -> IO ()
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

scrollDown :: (IORef Search, Lock) -> Int -> Int -> IO ()
scrollDown (ref, lock) cols cnt = Lock.with lock $ scrollDown' ref cols cnt

scrollDown' :: IORef Search -> Int -> Int -> IO ()
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

selectPrev :: (IORef Search, Lock) -> Int -> IO ()
selectPrev (ref, lock) cnt = Lock.with lock $ selectPrev' ref cnt

selectPrev' :: IORef Search -> Int -> IO ()
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

selectNext :: (IORef Search, Lock) -> Int -> Int -> IO ()
selectNext (ref, lock) cols cnt = Lock.with lock $ selectNext' ref cols cnt

selectNext' :: IORef Search -> Int -> Int -> IO ()
selectNext' ref cols cnt = do
    buf <- readIORef ref

    case listSelectNext (bufferList buf) (toRegion cols 0) cnt of
        Just ls -> do
            debugM rootLoggerName $ "old: " ++ show (listSelected $ bufferList buf) ++ " " ++
                                    "new: " ++ show (listSelected ls) ++ " " ++
                                    "oldhead: " ++ show (listDisplayHead $ bufferList buf) ++ " " ++
                                    "newhead: " ++ show (listDisplayHead ls)
            writeIORef ref buf { bufferList      = ls
                                      , bufferStatusBar = (bufferStatusBar buf) { sBarCurrent = listSelected ls }
                                      }
        Nothing -> do
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

    matched    <- NM.threadMatchedMessages t
    total      <- NM.threadTotalMessages t
    authors    <- NM.threadAuthors t
    subject    <- NM.threadSubject t
    oldestDate <- NM.threadOldestDate t
    newestDate <- NM.threadNewestDate t

    tags <- NM.threadTags t
    tagList <- tagsToList tags
    NM.tagsDestroy tags

    oldestDateRelative <- relativeTime oldestDate
    newestDateRelative <- relativeTime newestDate

    let theme = bufferTheme buf
        len   = listLength $ bufferList buf
        ld    = LineData { threadOldestDate = (oldestDate, oldestDateRelative)
                         , threadNewestDate = (newestDate, newestDateRelative)
                         , threadMatched    = matched
                         , threadTotal      = total
                         , threadAuthors    = decodeString authors
                         , threadSubject    = decodeString subject
                         , threadTags       = tagList
                         }
        line  = Line { lineData   = ld
                     , lineRender = (themeDrawLine theme) (themeAttrs theme)
                     }

    writeIORef ref buf { bufferList      = listAppend (bufferList buf) line
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
        msg <- themeFormatLoading (bufferTheme buf) term
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
        msg <- themeFormatLoadingDone (bufferTheme buf) term
        writeIORef ref buf { bufferStatusMessage = (bufferStatusMessage buf) { sMessage = msg }
                           }
        putMVar mvar Redraw

-- vim: set ft=haskell et ts=4 sts=4 sw=4 fdm=marker :
