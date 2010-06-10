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
    ( SearchAttr(..), SearchBar(..), SearchMsg(..), SearchLine(..), SearchWindow(..)
    , emptySearchWindow
    , scrollBackward, scrollForward
    , selectPrev, selectNext
    , paintSearchWindow
    , queryThreads
    ) where

import Control.Concurrent.MVar  (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Monad            (when)
import Data.IORef               (IORef, readIORef, writeIORef)
import Data.String.Utils        (join)
import Text.Printf              (printf)
import Debug.Trace              (putTraceMsg)

import Codec.Binary.UTF8.String (decodeString)

import Graphics.Vty
    ( Attr, Image, Picture
    , def_attr, with_back_color, with_fore_color
    , black, white, cyan, yellow
    , bright_black, bright_white, bright_blue, bright_yellow
    , char, string
    , horiz_cat, vert_cat
    , pic_for_image
    )

import Email.Notmuch
    ( Database, DatabaseOpenMode(..), databaseOpen, databaseClose
    , Query, queryCreate, querySearchThreads
    , Threads, threadsValid, threadsGet, threadsMoveToNext, threadsDestroy
    , Thread, threadMatchedMessages, threadTotalMessages
    , threadAuthors, threadSubject, threadTags, threadDestroy
    , Tags, tagsDestroy
    )

import qualified Qoropa.Lock as Lock (with)
import Qoropa.Notmuch (tagsToList)
import Qoropa.Util    (beep)
import {-# SOURCE #-} Qoropa.UI ( UI(..), UIEvent(..) )

data SearchAttr = SearchAttr
    { attrBar     :: Attr
    , attrMsg     :: Attr
    , attrSel     :: Attr
    , attrDef     :: Attr
    , attrEmpty   :: Attr
    , attrTag     :: Attr
    , attrSelTag  :: Attr
    , attrNum     :: Attr
    , attrSelNum  :: Attr
    }

data SearchBar = SearchBar
    { bTerm     :: String
    , bCurrent  :: Int
    , bTotal    :: Int
    }

data SearchMsg = SearchMsg { mMsg :: String }

data SearchLine = SearchLine
    { lIndex    :: Int
    , tMatched  :: Integer
    , tTotal    :: Integer
    , tAuthors  :: String
    , tSubject  :: String
    , tTags     :: [String]
    }

data SearchWindow = SearchWindow
    { wFirst     :: Int
    , wSelected  :: Int
    , wLines     :: [SearchLine]
    , wBar       :: SearchBar
    , wMsg       :: SearchMsg
    , wAttr      :: SearchAttr
    }

defaultSearchAttr :: SearchAttr
defaultSearchAttr = SearchAttr { attrBar    = def_attr `with_back_color` bright_white `with_fore_color` bright_blue
                               , attrMsg    = def_attr `with_back_color` black `with_fore_color` bright_yellow
                               , attrSel    = def_attr `with_back_color` cyan `with_fore_color` black
                               , attrDef    = def_attr `with_back_color` black `with_fore_color` white
                               , attrEmpty  = def_attr `with_back_color` black `with_fore_color` cyan
                               , attrTag    = def_attr `with_back_color` black `with_fore_color` yellow
                               , attrSelTag = def_attr `with_back_color` cyan `with_fore_color` bright_yellow
                               , attrNum    = def_attr `with_back_color` black `with_fore_color` bright_white
                               , attrSelNum = def_attr `with_back_color` cyan `with_fore_color` bright_black
                               }

emptySearchBar :: SearchBar
emptySearchBar = SearchBar { bTerm    = ""
                           , bCurrent = 1
                           , bTotal   = 0
                           }

emptySearchMsg :: SearchMsg
emptySearchMsg = SearchMsg { mMsg = " " }

emptySearchWindow :: SearchWindow
emptySearchWindow = SearchWindow { wFirst     = 1
                                 , wSelected  = 1
                                 , wLines     = []
                                 , wBar       = emptySearchBar
                                 , wMsg       = emptySearchMsg
                                 , wAttr      = defaultSearchAttr
                                 }

drawSearchLine :: SearchAttr -> Int -> SearchLine -> Image
drawSearchLine attr selected line =
    horiz_cat [ string myNumAttr myNumFmt
              , char myDefAttr ' '
              , string myDefAttr myDefFmt
              , char myDefAttr ' '
              , string myTagAttr myTagFmt
              ]
    where
        myNumAttr = if selected == lIndex line then attrSelNum attr else attrNum attr
        myDefAttr = if selected == lIndex line then attrSel attr else attrDef attr
        myTagAttr = if selected == lIndex line then attrSelTag attr else attrTag attr
        myNumFmt = printf " %d/%d" (tMatched line) (tTotal line)
        myDefFmt = printf "%s - %s" (tAuthors line) (tSubject line)
        myTagFmt = join " " $ map ('+' :) (tTags line)

drawSearchBar :: SearchAttr -> SearchBar -> Image
drawSearchBar attr bar =
    string myattr myfmt
    where
        myattr = attrBar attr
        myfmt  = bTerm bar ++ " [" ++ show (bCurrent bar) ++ "/" ++ show (bTotal bar) ++ "]"

drawSearchMsg :: SearchAttr -> SearchMsg -> Image
drawSearchMsg attr msg = string (attrMsg attr) (mMsg msg)

paintSearchWindow :: SearchWindow -> Int -> Picture
paintSearchWindow win h =
    pic_for_image $ vert_cat $ lines ++ fill ++ [bar, msg]
    where
        lines = take (h - 2) $ drop (wFirst win - 1) $ map (drawSearchLine (wAttr win) (wSelected win)) (wLines win)
        fill  = if length lines < h - 2
            then replicate (h - 2 - length lines) (string (attrEmpty $ wAttr win) "~")
            else []
        bar   = drawSearchBar (wAttr win) (wBar win)
        msg   = drawSearchMsg (wAttr win) (wMsg win)

scrollBackward :: UI -> Int -> IO ()
scrollBackward ui count = Lock.with (searchWinLock ui) (scrollBackward' ui count)

scrollBackward' :: UI -> Int -> IO ()
scrollBackward' ui count = do
    win <- readIORef (searchWin ui)
    let first = wFirst win - count

    if first > 0
        then writeIORef (searchWin ui) win { wFirst = first
                                           , wSelected = wSelected win - count + 1
                                           , wBar = (wBar win) { bCurrent = wSelected win - count + 1 }
                                           }
        else do
            beep
            writeIORef (searchWin ui) win { wMsg = (wMsg win) { mMsg = "Hit the top!" }
                                          }

    putMVar (uiEvent ui) RedrawSearch

scrollForward :: UI -> Int -> IO ()
scrollForward ui count = Lock.with (searchWinLock ui) (scrollForward' ui count)

scrollForward' :: UI -> Int -> IO ()
scrollForward' ui count = do
    win <- readIORef (searchWin ui)
    (cols, _) <- readIORef (scrSize ui)
    let len = length $ wLines win
    let first = wFirst win + count

    if first + cols - 3 <= len
        then writeIORef (searchWin ui) win { wFirst = first
                                           , wSelected = wSelected win + count - 1
                                           , wBar = (wBar win) { bCurrent = wSelected win + count - 1}
                                           }
        else do
            beep
            writeIORef (searchWin ui) win { wMsg = (wMsg win) { mMsg = "Hit the bottom!" }
                                          }

    putMVar (uiEvent ui) RedrawSearch

selectPrev :: UI -> Int -> IO ()
selectPrev ui count = Lock.with (searchWinLock ui) (selectPrev' ui count)

selectPrev' :: UI -> Int -> IO ()
selectPrev' ui count = do
    win <- readIORef (searchWin ui)
    let sel = wSelected win
    let first = wFirst win

    if sel - count >= 1
        then do
            writeIORef (searchWin ui) win { wSelected = sel - count
                                          , wBar = (wBar win) { bCurrent = sel - count }
                                          }
            when (sel - count < first) $ scrollBackward' ui count
        else do
            beep
            writeIORef (searchWin ui) win { wMsg = (wMsg win) { mMsg = "Hit the top!" }
                                          }

    putMVar (uiEvent ui) RedrawSearch

selectNext :: UI -> Int -> IO ()
selectNext ui count = Lock.with (searchWinLock ui) (selectNext' ui count)

selectNext' :: UI -> Int -> IO ()
selectNext' ui count = do
    win <- readIORef (searchWin ui)
    (cols, _) <- readIORef (scrSize ui)
    let sel = wSelected win
    let len = length $ wLines win
    let last = wFirst win + cols - 3

    if sel + count <= len
        then do
            writeIORef (searchWin ui) win { wSelected = sel + count
                                          , wBar = (wBar win) { bCurrent = sel + count }
                                          }
            when (sel + count > last) $ scrollForward' ui count
        else do
            beep
            writeIORef (searchWin ui) win { wMsg = (wMsg win) { mMsg = "Hit the bottom!" }
                                          }

    putMVar (uiEvent ui) RedrawSearch

addThread :: UI -> Thread -> IO ()
addThread ui t = do
    win <- readIORef (searchWin ui)

    let ind = length $ wLines win

    matched <- threadMatchedMessages t
    total <- threadTotalMessages t
    authors <- threadAuthors t
    subject <- threadSubject t

    tags <- threadTags t
    taglist <- tagsToList tags
    tagsDestroy tags

    let line = SearchLine { lIndex    = ind + 1
                          , tMatched  = matched
                          , tTotal    = total
                          , tAuthors  = decodeString authors
                          , tSubject  = decodeString subject
                          , tTags     = taglist
                          }

    writeIORef (searchWin ui) win { wLines = wLines win ++ [line]
                                  , wBar = (wBar win) { bTotal = ind + 1 }
                                  }

    putMVar (uiEvent ui) RedrawSearch

addThreads :: UI -> Threads -> IO ()
addThreads ui ts = do
    v <- threadsValid ts
    when v $ do
        (Just t) <- threadsGet ts
        Lock.with (searchWinLock ui) (addThread ui t)
        threadDestroy t >> threadsMoveToNext ts >> addThreads ui ts

queryThreads :: UI -> FilePath -> String -> IO ()
queryThreads ui fp term = do
    Lock.with (searchWinLock ui) (do
        win <- readIORef (searchWin ui)
        writeIORef (searchWin ui) win { wBar = (wBar win) { bTerm = term }
                                      , wMsg = (wMsg win) { mMsg = "Loading " ++ term ++ " ..." }
                                      }
        putMVar (uiEvent ui) RedrawSearch)

    (Just db) <- databaseOpen fp ModeReadOnly
    (Just query) <- queryCreate db term
    threads <- querySearchThreads query
    addThreads ui threads
    databaseClose db

    Lock.with (searchWinLock ui) (do
        win <- readIORef (searchWin ui)
        writeIORef (searchWin ui) win { wMsg = (wMsg win) { mMsg = "Done loading " ++ term }
                                      }
        putMVar (uiEvent ui) RedrawSearch)

-- vim: set ft=haskell et ts=4 sts=4 sw=4 fdm=marker :
