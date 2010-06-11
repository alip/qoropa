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

module Qoropa.UI
    ( UI(..), UIEvent(..)
    , redraw, exit, currentBuffer
    , scrollUp, scrollDown, selectPrev, selectNext
    ) where

import Control.Concurrent      (ThreadId)
import Control.Concurrent.MVar (MVar)
import Data.IORef              (IORef)

import Data.Sequence (Seq)
import Graphics.Vty  (Vty, Event)

import Qoropa.Lock                  (Lock)
import {-# SOURCE #-} Qoropa.Buffer (Buffer)

data UI = UI
    { vty        :: Vty
    , uiEvent    :: MVar UIEvent
    , uiThread   :: ThreadId
    , scrSize    :: IORef (Int, Int)
    , bufSeq     :: IORef (Seq (Buffer, Lock))
    , bufCurrent :: IORef Int
    }

data UIEvent = VtyEvent Event
               | NewFolder
               | NewSearch String
               | Redraw
               | Exit

redraw :: UI -> IO ()
exit :: UI -> IO ()
currentBuffer :: UI -> IO (Buffer, Lock)
scrollUp :: Int -> UI -> IO ()
scrollDown :: Int -> UI -> IO ()
selectPrev :: Int -> UI -> IO ()
selectNext :: Int -> UI -> IO ()

-- vim: set ft=haskell et ts=4 sts=4 sw=4 fdm=marker :
