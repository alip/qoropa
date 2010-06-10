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
    , defaultConfig
    ) where

import Data.Map (Map)
import qualified Data.Map as Map
import Graphics.Vty (Event(..), Key(..), Modifier(..))

import {-# SOURCE #-} Qoropa.UI
    ( UI(..)
    , redraw, exit
    , selectPrev, selectNext
    )

data QoropaConfig = QoropaConfig
    { databasePath :: FilePath
    , folderList   :: [(String,String)]
    , keys         :: Map Event (UI -> IO ())
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
    ]

defaultConfig :: QoropaConfig
defaultConfig = QoropaConfig
    { databasePath = "~/.maildir"
    , folderList   = [("inbox", "tag:inbox")]
    , keys         = defaultKeys
    }

-- vim: set ft=haskell et ts=4 sts=4 sw=4 fdm=marker :
