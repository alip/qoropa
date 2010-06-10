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

module Qoropa
    ( module Qoropa.Config
    , module Qoropa.Util
    , module Qoropa.Notmuch
    , module Qoropa.Buffer
    , module Qoropa.UI
    , qoropa
    ) where

import Qoropa.Config
import Qoropa.Util
import Qoropa.Notmuch
import Qoropa.Buffer
import Qoropa.UI

import Control.Concurrent (forkIO)

qoropa :: QoropaConfig -> IO ()
qoropa conf = do
    ui   <- start
    path <- expandTilde (databasePath conf)
    let term = "tag:inbox"

    forkIO (queryThreads ui path term)

    mainLoop ui

-- vim: set ft=haskell et ts=4 sts=4 sw=4 fdm=marker :
