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
    , defaultSearchAttributes, defaultSearchTheme
    , searchDrawLine, searchDrawStatusBar, searchDrawStatusMessage
    ) where

import Data.String.Utils (join)
import Text.Printf       (printf)

import Data.Map (Map)
import qualified Data.Map as Map

import Graphics.Vty
    ( Event(..), Key(..), Modifier(..), Image
    , string, char
    , horiz_cat
    , def_attr, with_back_color, with_fore_color
    , black, white, magenta, cyan
    , bright_black, bright_white, bright_magenta, bright_blue, bright_yellow
    )

import Qoropa.Util (beep)
import {-# SOURCE #-} Qoropa.UI
    ( UI(..)
    , redraw, exit
    , selectPrev, selectNext
    , scrollDown, scrollUp
    )
import qualified Qoropa.Buffer.Search as Search
    ( Attributes(..), Theme(..), Line(..), StatusBar(..), StatusMessage(..) )

defaultSearchAttributes :: Search.Attributes
defaultSearchAttributes = Search.Attributes
    { Search.attrStatusBar      = def_attr `with_back_color` bright_white `with_fore_color` bright_blue
    , Search.attrStatusMessage  = def_attr `with_back_color` black `with_fore_color` bright_yellow
    , Search.attrSelected       = def_attr `with_back_color` cyan `with_fore_color` black
    , Search.attrDefault        = def_attr `with_back_color` black `with_fore_color` white
    , Search.attrEmpty          = def_attr `with_back_color` black `with_fore_color` cyan
    , Search.attrTag            = def_attr `with_back_color` black `with_fore_color` magenta
    , Search.attrSelectedTag    = def_attr `with_back_color` cyan `with_fore_color` bright_magenta
    , Search.attrNumber         = def_attr `with_back_color` black `with_fore_color` bright_white
    , Search.attrSelectedNumber = def_attr `with_back_color` cyan `with_fore_color` bright_black
    }

defaultSearchTheme :: Search.Theme
defaultSearchTheme = Search.Theme
    { Search.themeAttrs              = defaultSearchAttributes
    , Search.themeEmptyFill          = "~"
    , Search.themeDrawLine           = searchDrawLine
    , Search.themeDrawStatusBar      = searchDrawStatusBar
    , Search.themeDrawStatusMessage  = searchDrawStatusMessage
    , Search.themeFormatHitTheTop    = beep >> return "Hit the top!"
    , Search.themeFormatHitTheBottom = beep >> return "Hit the bottom!"
    , Search.themeFormatLoading      = (\term -> return $ "Loading " ++ term ++ " ...")
    , Search.themeFormatLoadingDone  = (\term -> return $ "Done loading " ++ term)
    }

searchDrawLine :: Search.Attributes -> Int -> Search.Line -> Image
searchDrawLine attr selected line =
    horiz_cat [ string myNumberAttribute myNumberFormat
              , char myDefaultAttribute ' '
              , string myDefaultAttribute myDefaultFormat
              , char myDefaultAttribute ' '
              , string myTagAttribute myTagFormat
              ]
    where
        myNumberAttribute  = if selected == Search.lineIndex line
            then Search.attrSelectedNumber attr
            else Search.attrNumber attr
        myDefaultAttribute = if selected == Search.lineIndex line
            then Search.attrSelected attr
            else Search.attrDefault attr
        myTagAttribute     = if selected == Search.lineIndex line
            then Search.attrSelectedTag attr
            else Search.attrTag attr
        myNumberFormat     = printf " %d/%d" (Search.threadMatched line) (Search.threadTotal line)
        myDefaultFormat    = printf "%s - %s" (Search.threadAuthors line) (Search.threadSubject line)
        myTagFormat        = join " " $ map ('+' :) (Search.threadTags line)

searchDrawStatusBar :: Search.Attributes -> Search.StatusBar -> Image
searchDrawStatusBar attr bar =
    string myAttribute myFormat
    where
        myAttribute = Search.attrStatusBar attr
        myFormat  = Search.sBarTerm bar ++
            " [" ++ show (Search.sBarCurrent bar) ++
            "/" ++ show (Search.sBarTotal bar) ++ "]"

searchDrawStatusMessage :: Search.Attributes -> Search.StatusMessage -> Image
searchDrawStatusMessage attr msg = string (Search.attrStatusMessage attr) (Search.sMessage msg)

data QoropaConfig = QoropaConfig
    { databasePath :: FilePath
    , folderList   :: [(String,String)]
    , keys         :: Map Event (UI -> IO ())
    , themeSearch  :: Search.Theme
    }

defaultKeys :: Map Event (UI -> IO ())
defaultKeys = Map.fromList
    [ ( EvKey (KASCII 'l') [MCtrl], redraw        )
    , ( EvKey (KASCII 'q') [],      exit          )
    , ( EvKey (KASCII 'j') [],      selectNext 1  )
    , ( EvKey (KASCII 'k') [],      selectPrev 1  )
    , ( EvKey KUp [],               selectPrev 1  )
    , ( EvKey KDown [],             selectNext 1  )
    , ( EvKey KPageUp [],           selectPrev 10 )
    , ( EvKey KPageDown [],         selectNext 10 )
    , ( EvKey (KASCII 'n') [],      scrollDown 1  )
    , ( EvKey (KASCII 'o') [],      scrollUp 1    )
    ]

defaultConfig :: QoropaConfig
defaultConfig = QoropaConfig
    { databasePath = "~/.maildir"
    , folderList   = [("inbox", "tag:inbox")]
    , keys         = defaultKeys
    , themeSearch  = defaultSearchTheme
    }

-- vim: set ft=haskell et ts=4 sts=4 sw=4 fdm=marker :
