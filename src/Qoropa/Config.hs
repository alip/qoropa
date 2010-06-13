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
    , defaultLogAttributes, defaultLogTheme
    , logDrawLine, logDrawStatusBar, logDrawStatusMessage
    , defaultSearchAttributes, defaultSearchTheme
    , searchDrawLine, searchDrawStatusBar, searchDrawStatusMessage
    ) where

import Data.Char         (chr)
import Data.List         (intersperse)
import Data.String.Utils (join)
import Data.Time         (formatTime)
import System.Locale     (defaultTimeLocale)
import System.Log        (Priority(..))
import Text.Printf       (printf)

import Data.Map (Map)
import qualified Data.Map as Map

import Graphics.Vty
    ( Event(..), Key(..), Modifier(..), Image
    , string, char, empty_image
    , horiz_cat
    , def_attr, with_back_color, with_fore_color, with_style
    , bold
    , black, white, magenta, yellow, cyan, red, green, blue
    , bright_black, bright_blue, bright_yellow
    )

import Qoropa.Notmuch (splitAuthors)
import Qoropa.Util    (beep)
import {-# SOURCE #-} Qoropa.UI
    ( UI(..)
    , redraw, exit
    , selectPrev, selectNext
    , openSelected, cancelOperation
    , switchBuffer, switchBufferNext, switchBufferPrev
    )

import qualified Qoropa.Buffer.Folder as Folder
    ( Attributes(..), Theme(..), LineData(..), StatusBar(..), StatusMessage(..) )
import qualified Qoropa.Buffer.Log as Log
    ( Attributes(..), Theme(..), LineData(..), StatusBar(..), StatusMessage(..) )
import qualified Qoropa.Buffer.Search as Search
    ( Attributes(..), Theme(..), LineData(..), StatusBar(..), StatusMessage(..) )

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
    , Folder.themeFill               = Just "~"
    , Folder.themeDrawLine           = folderDrawLine
    , Folder.themeDrawStatusBar      = folderDrawStatusBar
    , Folder.themeDrawStatusMessage  = folderDrawStatusMessage
    , Folder.themeFormatHitTheTop    = beep >> return "Hit the top!"
    , Folder.themeFormatHitTheBottom = beep >> return "Hit the bottom!"
    , Folder.themeFormatLoading      = (\(name, term) -> return $ "Loading " ++ name ++ " ( " ++ term ++ " )...")
    , Folder.themeFormatLoadingDone  = (\(name, term) -> return $ "Done loading " ++ name ++ " ( " ++ term ++ " )")
    }

folderDrawLine :: Folder.Attributes -> Folder.LineData -> Bool -> Image
folderDrawLine attr ld sel =
    horiz_cat $ intersperse (char myDefaultAttribute ' ')
        [ string myNameAttribute myNameFormat
        , string myCountAttribute myCountFormat
        , string myTermAttribute myTermFormat
        ]
    where
        f = if sel then snd else fst
        myDefaultAttribute = f $ Folder.attrDefault attr
        myNameAttribute    = f $ Folder.attrName attr
        myTermAttribute    = f $ Folder.attrTerm attr
        myCountAttribute   = f $ Folder.attrCount attr
        myNameFormat  = printf "%-20s" (Folder.folderName ld)
        myCountFormat = printf "%7d" (Folder.folderCount ld)
        myTermFormat  = Folder.folderTerm ld

folderDrawStatusBar :: Folder.Attributes -> Folder.StatusBar -> Image
folderDrawStatusBar attr bar =
    string myAttribute myFormat
    where
        myAttribute = Folder.attrStatusBar attr
        myFormat  = "[Qoropa.Buffer.Folder] " ++
            "[" ++ show (Folder.sBarCurrent bar + 1) ++
            "/" ++ show (Folder.sBarTotal bar) ++ "]"


folderDrawStatusMessage :: Folder.Attributes -> Folder.StatusMessage -> Image
folderDrawStatusMessage attr msg = string (Folder.attrStatusMessage attr) (Folder.sMessage msg)

defaultLogAttributes :: Log.Attributes
defaultLogAttributes = Log.Attributes
    { Log.attrStatusBar     = def_attr `with_back_color` green `with_fore_color` black
    , Log.attrStatusMessage = def_attr `with_fore_color` bright_yellow
    , Log.attrFill          = def_attr `with_fore_color` cyan
    , Log.attrTime          = ( def_attr `with_fore_color` white
                              , def_attr `with_back_color` yellow `with_fore_color` black
                              )
    , Log.attrDomain        = ( def_attr `with_fore_color` magenta
                              , def_attr `with_back_color` yellow `with_fore_color` magenta `with_style` bold
                              )
    , Log.attrPriority      = ( def_attr `with_fore_color` green
                              , def_attr `with_back_color` yellow `with_fore_color` blue `with_style` bold
                              )
    , Log.attrMessage       = ( def_attr `with_fore_color` white
                              , def_attr `with_back_color` yellow `with_fore_color` black
                              )
    , Log.attrDefault       = ( def_attr
                              , def_attr `with_back_color` yellow
                              )
    }

defaultLogTheme :: Log.Theme
defaultLogTheme = Log.Theme
    { Log.themeAttrs              = defaultLogAttributes
    , Log.themeFill               = Just "~"
    , Log.themeDrawLine           = logDrawLine
    , Log.themeDrawStatusBar      = logDrawStatusBar
    , Log.themeDrawStatusMessage  = logDrawStatusMessage
    , Log.themeFormatHitTheTop    = beep >> return "Hit the top!"
    , Log.themeFormatHitTheBottom = beep >> return "Hit the bottom!"
    }

logDrawLine :: Log.Attributes -> Log.LineData -> Bool -> Image
logDrawLine attr ld sel =
    horiz_cat $ intersperse (char myDefaultAttribute ' ')
        [ string myTimeAttribute myTimeFormat
        , string myDomainAttribute myDomainFormat
        , string myPriorityAttribute myPriorityFormat
        , string myMessageAttribute myMessageFormat
        ]
    where
        f = if sel then snd else fst
        myDefaultAttribute  = f $ Log.attrDefault attr
        myTimeAttribute     = f $ Log.attrTime attr
        myDomainAttribute   = f $ Log.attrDomain attr
        myPriorityAttribute = f $ Log.attrPriority attr
        myMessageAttribute  = f $ Log.attrMessage attr
        myTimeFormat        = formatTime defaultTimeLocale "%Y-%m-%d %T %z" $ Log.lineDataTime ld
        myDomainFormat      = printf "%-15s" $ Log.lineDataDomain ld
        myPriorityFormat    = printf "%-7s" (show $ fst $ Log.lineDataRecord ld)
        myMessageFormat     = snd $ Log.lineDataRecord ld

logDrawStatusBar :: Log.Attributes -> Log.StatusBar -> Image
logDrawStatusBar attr bar =
    string myAttribute myFormat
    where
        myAttribute = Log.attrStatusBar attr
        myFormat  = "[Qoropa.Buffer.Log] " ++
            "[" ++ show (Log.sBarCurrent bar + 1) ++
            "/" ++ show (Log.sBarTotal bar) ++ "]"

logDrawStatusMessage :: Log.Attributes -> Log.StatusMessage -> Image
logDrawStatusMessage attr msg = string (Log.attrStatusMessage attr) (Log.sMessage msg)

defaultSearchAttributes :: Search.Attributes
defaultSearchAttributes = Search.Attributes
    { Search.attrStatusBar        = def_attr `with_back_color` green `with_fore_color` black
    , Search.attrStatusMessage    = def_attr `with_fore_color` bright_yellow
    , Search.attrFill             = def_attr `with_fore_color` cyan
    , Search.attrTime             = ( def_attr `with_fore_color` white
                                    , def_attr `with_back_color` yellow `with_fore_color` black
                                    )
    , Search.attrCount            = ( def_attr `with_fore_color` white
                                    , def_attr `with_back_color` yellow `with_fore_color` black
                                    )
    , Search.attrAuthorMatched    = ( def_attr `with_fore_color` green
                                    , def_attr `with_back_color` yellow `with_fore_color` blue `with_style` bold
                                    )
    , Search.attrAuthorNonMatched = ( def_attr `with_fore_color` bright_black
                                    , def_attr `with_back_color` yellow `with_fore_color` bright_black
                                    )
    , Search.attrSubject          = ( def_attr `with_fore_color` white
                                    , def_attr `with_back_color` yellow `with_fore_color` blue
                                    )
    , Search.attrTag              = ( def_attr `with_fore_color` magenta
                                    , def_attr `with_back_color` yellow `with_fore_color` blue `with_style` bold
                                    )
    , Search.attrDefault          = ( def_attr
                                    , def_attr `with_back_color` yellow
                                    )
    }

defaultSearchTheme :: Search.Theme
defaultSearchTheme = Search.Theme
    { Search.themeAttrs              = defaultSearchAttributes
    , Search.themeFill               = Just "~"
    , Search.themeDrawLine           = searchDrawLine
    , Search.themeDrawStatusBar      = searchDrawStatusBar
    , Search.themeDrawStatusMessage  = searchDrawStatusMessage
    , Search.themeFormatHitTheTop    = beep >> return "Hit the top!"
    , Search.themeFormatHitTheBottom = beep >> return "Hit the bottom!"
    , Search.themeFormatLoading      = (\term -> return $ "Loading " ++ term ++ " ... (Hit ^C to cancel)")
    , Search.themeFormatLoadingDone  = (\term -> return $ "Done loading " ++ term)
    }

searchDrawLine :: Search.Attributes -> Search.LineData -> Bool -> Image
searchDrawLine attr ld sel =
    horiz_cat $ intersperse (char myDefaultAttribute ' ')
        [ string myTimeAttribute myTimeFormat
        , string myCountAttribute myCountFormat
        , myAuthorString
        , string mySubjectAttribute mySubjectFormat
        , string myTagAttribute myTagFormat
        ]
    where
        f = if sel then snd else fst
        myDefaultAttribute = f $ Search.attrDefault attr
        myTimeAttribute    = f $ Search.attrTime attr
        myCountAttribute   = f $ Search.attrCount attr
        mySubjectAttribute = f $ Search.attrSubject attr
        myTagAttribute     = f $ Search.attrTag attr
        myTimeFormat  = printf "%15s" (snd $ Search.threadNewestDate ld)
        myCountFormat = printf "%-10s" ( "[" ++ show (Search.threadMatched ld) ++
                                         "/" ++ show (Search.threadTotal ld) ++
                                         "]"
                                       )
        mySubjectFormat = printf "%-20s" (Search.threadSubject ld)
        myTagFormat     = join " " $ map ('+' :) (Search.threadTags ld)

        myAuthorMatchedAttribute    = f $ Search.attrAuthorMatched attr
        myAuthorNonMatchedAttribute = f $ Search.attrAuthorNonMatched attr
        (myAuthorMatchedFormat, myAuthorNonMatchedFormat) = splitAuthors (Search.threadAuthors ld) 17
        myAuthorMatchedString       = string myAuthorMatchedAttribute myAuthorMatchedFormat
        myAuthorNonMatchedString    = string myAuthorNonMatchedAttribute myAuthorNonMatchedFormat
        mySplitChar                 = if not (null myAuthorNonMatchedFormat)
            then char myAuthorMatchedAttribute ','
            else empty_image
        myAuthorString              = horiz_cat [myAuthorMatchedString, mySplitChar, myAuthorNonMatchedString]

searchDrawStatusBar :: Search.Attributes -> Search.StatusBar -> Image
searchDrawStatusBar attr bar =
    string myAttribute myFormat
    where
        myAttribute = Search.attrStatusBar attr
        myFormat  = "[Qoropa.Buffer.Search] " ++
            Search.sBarTerm bar ++
            " [" ++ show (Search.sBarCurrent bar + 1) ++
            "/" ++ show (Search.sBarTotal bar) ++ "]"

searchDrawStatusMessage :: Search.Attributes -> Search.StatusMessage -> Image
searchDrawStatusMessage attr msg = string (Search.attrStatusMessage attr) (Search.sMessage msg)

data QoropaConfig = QoropaConfig
    { configDatabasePath :: FilePath
    , configFolderList   :: [(String,String)]
    , configKeys         :: Map Event (UI -> IO ())
    , configLogPriority  :: Priority
    , configThemeSearch  :: Search.Theme
    , configThemeFolder  :: Folder.Theme
    , configThemeLog     :: Log.Theme
    , configVtyEscDelay  :: Int
    }

defaultKeys :: Map Event (UI -> IO ())
defaultKeys = Map.fromList $
    [ ( EvKey (KASCII 'l') [MCtrl], redraw           )
    , ( EvKey (KASCII 'q') [],      exit             )
    , ( EvKey (KASCII 'j') [],      selectNext 1     )
    , ( EvKey (KASCII 'k') [],      selectPrev 1     )
    , ( EvKey (KASCII 'J') [],      selectNext 5     )
    , ( EvKey (KASCII 'K') [],      selectPrev 5     )
    , ( EvKey KUp [],               selectPrev 1     )
    , ( EvKey KDown [],             selectNext 1     )
    , ( EvKey KEnter [],            openSelected     )
    , ( EvKey (KASCII 'c') [MCtrl], cancelOperation  )
    , ( EvKey (KASCII 'j') [MMeta], switchBufferNext )
    , ( EvKey (KASCII 'k') [MMeta], switchBufferPrev )
    ] ++
    -- Alt-[0..9], Switch to buffer N
    map (\i -> (EvKey (KASCII $ chr $ i + 48) [MMeta], switchBuffer i)) [0..9]

defaultConfig :: QoropaConfig
defaultConfig = QoropaConfig
    { configDatabasePath = "~/.maildir"
    , configFolderList   = [("inbox", "tag:inbox"), ("unread", "tag:inbox and tag:unread")]
    , configKeys         = defaultKeys
    , configLogPriority  = NOTICE
    , configThemeSearch  = defaultSearchTheme
    , configThemeFolder  = defaultFolderTheme
    , configThemeLog     = defaultLogTheme
    , configVtyEscDelay  = 1000
    }

-- vim: set ft=haskell et ts=4 sts=4 sw=4 fdm=marker :
