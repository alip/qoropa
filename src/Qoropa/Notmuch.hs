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
    ( splitAuthors
    , tagsToList
    ) where

import Data.String.Utils        (split)
import Codec.Binary.UTF8.String (decodeString)
import Email.Notmuch            (Tags, Tag, tagsValid, tagsGet, tagsMoveToNext)

splitAuthors :: String -> Int -> (String, String)
splitAuthors s i =
    (m', nm')
    where
        len  = length s
        trs  = take i s
        trsp = split "|" trs
        m    = trsp !! 0
        nm   = if length trsp > 1 then trsp !! 1 else []
        nm'  = if not (null nm)
            then (if len > i then nm ++ "..." else nm ++ (replicate (i + 3 - len) ' '))
            else nm
        m'   = if null nm
            then (if len > i then m ++ "..." else m ++ (replicate (i + 3 - len) ' '))
            else m

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
