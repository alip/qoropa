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

module Qoropa.Widget.List
    ( Line(..), List(..)
    , emptyList
    , listLength, listIndex, listAppend, listAppendLeft
    , listRender
    , listScrollUp, listScrollDown
    , listSelectPrev, listSelectNext
    , toRegion
    ) where

import Data.Maybe    (isJust, fromJust)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
    ( drop, empty, index, length, null, take
    , (|>), (<|)
    )

import Graphics.Vty
    ( DisplayRegion(..), Image
    )

data Line a = Line
    { lineData   :: a
    , lineRender :: a -> Bool -> Image
    }

data List a = List
    { listLines       :: Seq (Line a)
    , listDisplayHead :: Int
    , listSelected    :: Int
    , listLineFill    :: Maybe Image
    }

toRegion :: Int -> Int -> DisplayRegion
toRegion y x = DisplayRegion { region_height = toEnum y, region_width = toEnum x }

columns :: DisplayRegion -> Int
columns = fromIntegral . toInteger . region_height

emptyList :: List a
emptyList = List
    { listLines       = Seq.empty
    , listDisplayHead = 0
    , listSelected    = 0
    , listLineFill    = Nothing
    }

listLength :: List a -> Int
listLength ls = Seq.length $ listLines ls

listIndex :: List a -> Int -> Line a
listIndex ls i = Seq.index (listLines ls) i

listAppend :: List a -> Line a -> List a
listAppend ls l = ls { listLines = (listLines ls) Seq.|> l }

listAppendLeft :: List a -> Line a -> List a
listAppendLeft ls l = ls { listLines = l Seq.<| (listLines ls) }

linesRender :: Seq (Line a) -> Int -> [Image]
linesRender lns sel = linesRender' lns sel 0

linesRender' :: Seq (Line a) -> Int -> Int -> [Image]
linesRender' lns sel i
    | Seq.null lns  = []
    | otherwise     = myRender (Seq.index lns 0) : linesRender' (Seq.drop 1 lns) sel (i + 1)
    where
        myRender :: Line a -> Image
        myRender l = lineRender l (lineData l) (sel == i)

listRender :: List a -> DisplayRegion -> [Image]
listRender ls r =
    linesRender lns sel ++ fill
    where
        cols = columns r
        lns  = Seq.take cols $ Seq.drop (listDisplayHead ls) (listLines ls)
        sel  = listSelected ls - listDisplayHead ls
        len  = Seq.length lns
        fill = if isJust (listLineFill ls) && cols > len
            then replicate (cols - len) (fromJust $ listLineFill ls)
            else []

listScrollUp :: List a -> Int -> Maybe (List a)
listScrollUp ls cnt =
    let headd = listDisplayHead ls - cnt
        sel   = listSelected ls
    in

    if headd + 2 > 0
        then Just ls { listDisplayHead = headd
                     , listSelected    = sel - cnt + 1
                     }
        else Nothing


listScrollDown :: List a -> DisplayRegion -> Int -> Maybe (List a)
listScrollDown ls r cnt =
    let len   = listLength ls
        headd = listDisplayHead ls + cnt
        sel   = listSelected ls
        cols  = columns r
    in

    if headd + cols - 2 <= len
        then Just ls { listDisplayHead = headd
                     , listSelected    = sel + cnt
                     }
        else Nothing

listSelectPrev :: List a -> Int -> Maybe (List a)
listSelectPrev ls cnt =
    let headd = listDisplayHead ls
        sel   = listSelected ls
    in

    if sel - cnt >= 0
        then ( if sel - cnt < headd
                then listScrollUp ls { listSelected = sel - cnt } cnt
                else Just ls { listSelected = sel - cnt }
             )
        else Nothing

listSelectNext :: List a -> DisplayRegion -> Int -> Maybe (List a)
listSelectNext ls r cnt =
    let len = listLength ls
        sel = listSelected ls
        lst = listDisplayHead ls + columns r
    in

    if sel + cnt < len
        then ( if sel + cnt + 3 > lst
                then listScrollDown ls r cnt
                else Just ls { listSelected = sel + cnt }
             )
        else Nothing

-- vim: set ft=haskell et ts=4 sts=4 sw=4 fdm=marker :
