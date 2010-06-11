-- A test displaying all possible colours

import Data.List (intersperse, splitAt)
import Graphics.Vty

splitAll :: Int -> [a] -> [[a]]
splitAll i [] = []
splitAll i xs =
    h : splitAll i t
    where
        (h, t) = splitAt i xs

sep :: Image
sep = char def_attr ' '

normalColors :: [(String, Color)]
normalColors =
    [ ("red", red)
    , ("green", green)
    , ("blue", blue)
    , ("magenta", magenta)
    , ("cyan", cyan)
    , ("black", black)
    , ("white", white)
    ]
brightColors :: [(String, Color)]
brightColors =
    [ ("bright_red", bright_red)
    , ("bright_green", bright_green)
    , ("bright_blue", bright_blue)
    , ("bright_magenta", bright_magenta)
    , ("bright_cyan", bright_cyan)
    , ("bright_black", bright_black)
    , ("bright_white", bright_white)
    ]

lowColors :: Image
lowColors =
    vert_cat $ [horiz_cat n, horiz_cat b]
    where
        f = (\(s, c) -> string (def_attr `with_fore_color` c) s)
        n = intersperse sep $ map f normalColors
        b = intersperse sep $ map f brightColors

highColors :: Image
highColors =
    vert_cat cs
    where
        f  = (\i -> string (def_attr `with_fore_color` (ISOColor i)) (show i))
        ns = map (\l -> map f l) $ splitAll 16 [0..240]
        cs = map (\l -> horiz_cat $ intersperse sep l) ns

main :: IO ()
main = do
    vty <- mkVty
    update vty $ pic_for_image $ vert_cat [lowColors, string def_attr "---", highColors]
    getLine
    shutdown vty

-- vim: set ft=haskell et ts=4 sts=4 sw=4 fdm=marker :
