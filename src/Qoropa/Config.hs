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

module Qoropa.Config
    ( QoropaConfig(..)
    , defaultConfig, defaultKeys
    , defaultFolderAttributes, defaultFolderTheme
    , folderDrawLine, folderDrawStatusBar, folderDrawStatusMessage
    , defaultSearchAttributes, defaultSearchTheme
    , searchDrawLine, searchDrawStatusBar, searchDrawStatusMessage
    ) where

import Data.Char         (chr)
import Data.List         (intersperse)
import Data.String.Utils (join)
import Text.Printf       (printf)

import Data.Map (Map)
import qualified Data.Map as Map

import Graphics.Vty
    ( Event(..), Key(..), Modifier(..), Image
    , string, char
    , horiz_cat
    , def_attr, with_back_color, with_fore_color, with_style
    , bold
    , black, white, magenta, yellow, cyan, red, green, blue
    , bright_blue, bright_yellow
    )

import Qoropa.Util (beep)
import {-# SOURCE #-} Qoropa.UI
    ( UI(..)
    , redraw, exit
    , selectPrev, selectNext
    , openSelected, cancelOperation
    , switchBuffer, switchBufferNext, switchBufferPrev
    )

import qualified Qoropa.Buffer.Folder as Folder
    ( Attributes(..), Theme(..), Line(..), StatusBar(..), StatusMessage(..) )
import qualified Qoropa.Buffer.Search as Search
    ( Attributes(..), Theme(..), Line(..), StatusBar(..), StatusMessage(..) )

defaultFolderAttributes :: Folder.Attributes
defaultFolderAttributes = Folder.Attributes
    { Folder.attrStatusBar     = def_attr `with_back_color` green `with_fore_color` black
    , Folder.attrStatusMessage = def_attr `with_fore_color` bright_yellow
    , Folder.attrFill          = def_attr `with_fore_color` cyan
    , Folder.attrName          = ( def_attr `with_fore_color` white
                                 , def_attr `with_back_color` yellow `with_fore_color` black
                                 )
    , Folder.attrTerm          = ( def_attr `with_fore_color` green
                                 , def_attr `with_back_color` yellow `with_fore_color` bright_blue
                                 )
    , Folder.attrCount         = ( def_attr `with_fore_color` red
                                 , def_attr `with_back_color` yellow `with_fore_color` blue
                                 )
    , Folder.attrDefault       = ( def_attr
                                 , def_attr `with_back_color` yellow
                                 )
    }

defaultFolderTheme :: Folder.Theme
defaultFolderTheme = Folder.Theme
    { Folder.themeAttrs              = defaultFolderAttributes
    , Folder.themeFill               = "~"
    , Folder.themeDrawLine           = folderDrawLine
    , Folder.themeDrawStatusBar      = folderDrawStatusBar
    , Folder.themeDrawStatusMessage  = folderDrawStatusMessage
    , Folder.themeFormatHitTheTop    = beep >> return "Hit the top!"
    , Folder.themeFormatHitTheBottom = beep >> return "Hit the bottom!"
    , Folder.themeFormatLoading      = (\(name, term) -> return $ "Loading " ++ name ++ " ( " ++ term ++ " )...")
    , Folder.themeFormatLoadingDone  = (\(name, term) -> return $ "Done loading " ++ name ++ " ( " ++ term ++ " )")
    }

folderDrawLine :: Folder.Attributes -> Int -> Folder.Line -> Image
folderDrawLine attr selected line =
    horiz_cat $ intersperse (char myDefaultAttribute ' ')
        [ string myNameAttribute myNameFormat
        , string myCountAttribute myCountFormat
        , string myTermAttribute myTermFormat
        ]
    where
        myDefaultAttribute = if selected /= Folder.lineIndex line
            then fst $ Folder.attrDefault attr
            else snd $ Folder.attrDefault attr
        myNameAttribute = if selected /= Folder.lineIndex line
            then fst $ Folder.attrName attr
            else snd $ Folder.attrName attr
        myTermAttribute = if selected /= Folder.lineIndex line
            then fst $ Folder.attrTerm attr
            else snd $ Folder.attrTerm attr
        myCountAttribute = if selected /= Folder.lineIndex line
            then fst $ Folder.attrCount attr
            else snd $ Folder.attrCount attr
        myNameFormat  = printf "%-20s" (Folder.folderName line)
        myCountFormat = printf "%7d" (Folder.folderCount line)
        myTermFormat  = Folder.folderTerm line

folderDrawStatusBar :: Folder.Attributes -> Folder.StatusBar -> Image
folderDrawStatusBar attr bar =
    string myAttribute myFormat
    where
        myAttribute = Folder.attrStatusBar attr
        myFormat  = "[Qoropa.Buffer.Folder] " ++
            "[" ++ show (Folder.sBarCurrent bar) ++
            "/" ++ show (Folder.sBarTotal bar) ++ "]"


folderDrawStatusMessage :: Folder.Attributes -> Folder.StatusMessage -> Image
folderDrawStatusMessage attr msg = string (Folder.attrStatusMessage attr) (Folder.sMessage msg)

defaultSearchAttributes :: Search.Attributes
defaultSearchAttributes = Search.Attributes
    { Search.attrStatusBar     = def_attr `with_back_color` green `with_fore_color` black
    , Search.attrStatusMessage = def_attr `with_fore_color` bright_yellow
    , Search.attrFill          = def_attr `with_fore_color` cyan
    , Search.attrTime          = ( def_attr `with_fore_color` white
                                 , def_attr `with_back_color` yellow `with_fore_color` black
                                 )
    , Search.attrCount         = ( def_attr `with_fore_color` white
                                 , def_attr `with_back_color` yellow `with_fore_color` black
                                 )
    , Search.attrAuthor        = ( def_attr `with_fore_color` green
                                 , def_attr `with_back_color` yellow `with_fore_color` blue `with_style` bold
                                 )
    , Search.attrSubject       = ( def_attr `with_fore_color` white
                                 , def_attr `with_back_color` yellow `with_fore_color` blue
                                 )
    , Search.attrTag           = ( def_attr `with_fore_color` magenta
                                 , def_attr `with_back_color` yellow `with_fore_color` blue `with_style` bold
                                 )
    , Search.attrDefault       = ( def_attr
                                 , def_attr `with_back_color` yellow
                                 )
    }

defaultSearchTheme :: Search.Theme
defaultSearchTheme = Search.Theme
    { Search.themeAttrs              = defaultSearchAttributes
    , Search.themeFill               = "~"
    , Search.themeDrawLine           = searchDrawLine
    , Search.themeDrawStatusBar      = searchDrawStatusBar
    , Search.themeDrawStatusMessage  = searchDrawStatusMessage
    , Search.themeFormatHitTheTop    = beep >> return "Hit the top!"
    , Search.themeFormatHitTheBottom = beep >> return "Hit the bottom!"
    , Search.themeFormatLoading      = (\term -> return $ "Loading " ++ term ++ " ... (Hit ^C to cancel)")
    , Search.themeFormatLoadingDone  = (\term -> return $ "Done loading " ++ term)
    }

searchDrawLine :: Search.Attributes -> Int -> Search.Line -> Image
searchDrawLine attr selected line =
    horiz_cat $ intersperse (char myDefaultAttribute ' ')
        [ string myTimeAttribute myTimeFormat
        , string myCountAttribute myCountFormat
        , string myAuthorAttribute myAuthorFormat
        , string mySubjectAttribute mySubjectFormat
        , string myTagAttribute myTagFormat
        ]
    where
        myDefaultAttribute = if selected /= Search.lineIndex line
            then fst $ Search.attrDefault attr
            else snd $ Search.attrDefault attr
        myTimeAttribute = if selected /= Search.lineIndex line
            then fst $ Search.attrTime attr
            else snd $ Search.attrTime attr
        myCountAttribute   = if selected /= Search.lineIndex line
            then fst $ Search.attrCount attr
            else snd $ Search.attrCount attr
        myAuthorAttribute  = if selected /= Search.lineIndex line
            then fst $ Search.attrAuthor attr
            else snd $ Search.attrAuthor attr
        mySubjectAttribute = if selected /= Search.lineIndex line
            then fst $ Search.attrSubject attr
            else snd $ Search.attrSubject attr
        myTagAttribute     = if selected /= Search.lineIndex line
            then fst $ Search.attrTag attr
            else snd $ Search.attrTag attr
        myTimeFormat       = printf "%-s" (snd $ Search.threadNewestDate line)
        myCountFormat      = printf "[%d/%-d]" (Search.threadMatched line) (Search.threadTotal line)
        myAuthorFormat     = printf "%-10s" (Search.threadAuthors line)
        mySubjectFormat    = printf "%-20s" (Search.threadSubject line)
        myTagFormat        = join " " $ map ('+' :) (Search.threadTags line)

searchDrawStatusBar :: Search.Attributes -> Search.StatusBar -> Image
searchDrawStatusBar attr bar =
    string myAttribute myFormat
    where
        myAttribute = Search.attrStatusBar attr
        myFormat  = "[Qoropa.Buffer.Search] " ++
            Search.sBarTerm bar ++
            " [" ++ show (Search.sBarCurrent bar) ++
            "/" ++ show (Search.sBarTotal bar) ++ "]"

searchDrawStatusMessage :: Search.Attributes -> Search.StatusMessage -> Image
searchDrawStatusMessage attr msg = string (Search.attrStatusMessage attr) (Search.sMessage msg)

data QoropaConfig = QoropaConfig
    { databasePath :: FilePath
    , folderList   :: [(String,String)]
    , keys         :: Map Event (UI -> IO ())
    , themeSearch  :: Search.Theme
    , themeFolder  :: Folder.Theme
    }

defaultKeys :: Map Event (UI -> IO ())
defaultKeys = Map.fromList $
    [ ( EvKey (KASCII 'l') [MCtrl], redraw           )
    , ( EvKey (KASCII 'q') [],      exit             )
    , ( EvKey (KASCII 'j') [],      selectNext 1     )
    , ( EvKey (KASCII 'k') [],      selectPrev 1     )
    , ( EvKey KUp [],               selectPrev 1     )
    , ( EvKey KDown [],             selectNext 1     )
    , ( EvKey KEnter [],            openSelected     )
    , ( EvKey (KASCII 'c') [MCtrl], cancelOperation  )
    , ( EvKey (KASCII 'j') [MCtrl], switchBufferNext )
    , ( EvKey (KASCII 'k') [MCtrl], switchBufferPrev )
    ] ++
    -- Alt-[1..9], Switch to buffer N
    map (\i -> (EvKey (KASCII $ chr $ i + 48) [MMeta], switchBuffer i)) [1..9]

defaultConfig :: QoropaConfig
defaultConfig = QoropaConfig
    { databasePath = "~/.maildir"
    , folderList   = [("inbox", "tag:inbox"), ("unread", "tag:inbox and tag:unread")]
    , keys         = defaultKeys
    , themeSearch  = defaultSearchTheme
    , themeFolder  = defaultFolderTheme
    }

-- vim: set ft=haskell et ts=4 sts=4 sw=4 fdm=marker :
