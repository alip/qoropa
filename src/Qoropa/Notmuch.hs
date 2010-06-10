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

module Qoropa.Notmuch
    ( tagsToList
    ) where

import Codec.Binary.UTF8.String (decodeString)
import Email.Notmuch (Tags, Tag, tagsValid, tagsGet, tagsMoveToNext)

tagsToList :: Tags -> IO [Tag]
tagsToList ts = do
    v <- tagsValid ts
    if v
        then do
             t <- tagsGet ts
             tagsMoveToNext ts
             lst <- tagsToList ts
             return $ decodeString t : lst
        else return []

-- vim: set ft=haskell et ts=4 sts=4 sw=4 fdm=marker :
