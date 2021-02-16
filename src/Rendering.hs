module Rendering where

import           Control.Arrow             ((***))
import           Control.Monad             (join)
import           Data.Array
import           Graphics.Gloss
import           Graphics.Gloss.Data.Color ()

import           Game

itranslate :: (Int, Int) -> Picture -> Picture
itranslate (a, b) = translate (fromIntegral a) (fromIntegral b)

tmap :: (a -> b) -> (a, a) -> (b, b)
tmap = join (***)

render :: World -> Picture
render world = translate (fromIntegral screenWidth * (-0.5)) (fromIntegral screenHeight * (-0.5))
             $ pictures
             [ color violet $ marchingSquares world
             , color white  $ cells world Empty
             , color black  $ cells world Full
             ]

cells :: World -> Cell -> Picture
cells world cell = pictures
                 $ map (flip itranslate (circle 5) . tmap (* resolution) . fst)
                 $ filter (\(_, c) -> c == cell)
                 $ assocs world

marchingSquares :: World -> Picture
marchingSquares world = pictures
                      $ map ((\ (x, y) -> isolines ( world ! (x    , y    )
                                                   , world ! (x + 1, y    )
                                                   , world ! (x + 1, y + 1)
                                                   , world ! (x    , y + 1)
                                                   )
                                                   $ tmap (*resolution) (x, y))
                            . fst
                            )
                      $ filter (\((x, y), _) -> (x < fst (snd $ bounds world)) && (y < snd (snd $ bounds world)))
                      $ assocs world

isolines :: (Cell, Cell, Cell, Cell) -> (Int, Int) -> Picture
isolines (Empty, Empty, Empty, Empty) _ = Blank
isolines (Full , Empty, Empty, Empty) (x, y) = polygon [ ( fromIntegral x
                                                         , fromIntegral y
                                                         )
                                                       , ( fromIntegral x + fromIntegral resolution / 2
                                                         , fromIntegral y
                                                         )
                                                       , ( fromIntegral x
                                                         , fromIntegral y + fromIntegral resolution / 2
                                                         )
                                                       ]
isolines (Empty, Full , Empty, Empty) (x, y) = polygon [ ( fromIntegral x + fromIntegral resolution
                                                         , fromIntegral y
                                                         )
                                                       , ( fromIntegral x + fromIntegral resolution / 2
                                                         , fromIntegral y
                                                         )
                                                       , ( fromIntegral x + fromIntegral resolution
                                                         , fromIntegral y + fromIntegral resolution / 2
                                                         )
                                                       ]
isolines (Empty, Empty, Full , Empty) (x, y) = polygon [ ( fromIntegral x + fromIntegral resolution
                                                         , fromIntegral y + fromIntegral resolution
                                                         )
                                                       , ( fromIntegral x + fromIntegral resolution / 2
                                                         , fromIntegral y + fromIntegral resolution
                                                         )
                                                       , ( fromIntegral x + fromIntegral resolution
                                                         , fromIntegral y + fromIntegral resolution / 2
                                                         )
                                                       ]
isolines (Empty, Empty, Empty, Full ) (x, y) = polygon [ ( fromIntegral x
                                                         , fromIntegral y + fromIntegral resolution
                                                         )
                                                       , ( fromIntegral x + fromIntegral resolution / 2
                                                         , fromIntegral y + fromIntegral resolution
                                                         )
                                                       , ( fromIntegral x
                                                         , fromIntegral y + fromIntegral resolution / 2
                                                         )
                                                       ]
isolines (Full , Full , Empty, Empty) (x, y) = polygon [ ( fromIntegral x
                                                         , fromIntegral y
                                                         )
                                                       , ( fromIntegral x + fromIntegral resolution
                                                         , fromIntegral y
                                                         )
                                                       , ( fromIntegral x + fromIntegral resolution
                                                         , fromIntegral y + fromIntegral resolution / 2
                                                         )
                                                       , ( fromIntegral x
                                                         , fromIntegral y + fromIntegral resolution / 2
                                                         )
                                                       ]
isolines (Empty, Full , Full , Empty) (x, y) = polygon [ ( fromIntegral x + fromIntegral resolution
                                                         , fromIntegral y
                                                         )
                                                       , ( fromIntegral x + fromIntegral resolution
                                                         , fromIntegral y + fromIntegral resolution
                                                         )
                                                       , ( fromIntegral x + fromIntegral resolution / 2
                                                         , fromIntegral y + fromIntegral resolution
                                                         )
                                                       , ( fromIntegral x + fromIntegral resolution / 2
                                                         , fromIntegral y
                                                         )
                                                       ]
isolines (Empty, Empty, Full , Full ) (x, y) = polygon [ ( fromIntegral x + fromIntegral resolution
                                                         , fromIntegral y + fromIntegral resolution
                                                         )
                                                       , ( fromIntegral x
                                                         , fromIntegral y + fromIntegral resolution
                                                         )
                                                       , ( fromIntegral x
                                                         , fromIntegral y + fromIntegral resolution / 2
                                                         )
                                                       , ( fromIntegral x + fromIntegral resolution
                                                         , fromIntegral y + fromIntegral resolution / 2
                                                         )
                                                       ]
isolines (Full , Empty, Empty, Full ) (x, y) = polygon [ ( fromIntegral x
                                                         , fromIntegral y + fromIntegral resolution
                                                         )
                                                       , ( fromIntegral x
                                                         , fromIntegral y
                                                         )
                                                       , ( fromIntegral x + fromIntegral resolution / 2
                                                         , fromIntegral y
                                                         )
                                                       , ( fromIntegral x + fromIntegral resolution / 2
                                                         , fromIntegral y + fromIntegral resolution
                                                         )
                                                       ]
isolines (Full , Empty, Full , Empty) (x, y) = polygon [ ( fromIntegral x
                                                         , fromIntegral y
                                                         )
                                                       , ( fromIntegral x + fromIntegral resolution / 2
                                                         , fromIntegral y
                                                         )
                                                       , ( fromIntegral x + fromIntegral resolution
                                                         , fromIntegral y + fromIntegral resolution / 2
                                                         )
                                                       , ( fromIntegral x + fromIntegral resolution
                                                         , fromIntegral y + fromIntegral resolution
                                                         )
                                                       , ( fromIntegral x + fromIntegral resolution / 2
                                                         , fromIntegral y + fromIntegral resolution
                                                         )
                                                       , ( fromIntegral x
                                                         , fromIntegral y + fromIntegral resolution / 2
                                                         )
                                                       ]
isolines (Empty, Full , Empty, Full ) (x, y) = polygon [ ( fromIntegral x + fromIntegral resolution
                                                         , fromIntegral y
                                                         )
                                                       , ( fromIntegral x + fromIntegral resolution
                                                         , fromIntegral y + fromIntegral resolution / 2
                                                         )
                                                       , ( fromIntegral x + fromIntegral resolution / 2
                                                         , fromIntegral y + fromIntegral resolution
                                                         )
                                                       , ( fromIntegral x
                                                         , fromIntegral y + fromIntegral resolution
                                                         )
                                                       , ( fromIntegral x
                                                         , fromIntegral y + fromIntegral resolution / 2
                                                         )
                                                       , ( fromIntegral x + fromIntegral resolution / 2
                                                         , fromIntegral y
                                                         )
                                                       ]
isolines (Full , Full , Full , Empty) (x, y) = polygon [ ( fromIntegral x
                                                         , fromIntegral y
                                                         )
                                                       , ( fromIntegral x + fromIntegral resolution
                                                         , fromIntegral y
                                                         )
                                                       , ( fromIntegral x + fromIntegral resolution
                                                         , fromIntegral y + fromIntegral resolution
                                                         )
                                                       , ( fromIntegral x + fromIntegral resolution / 2
                                                         , fromIntegral y + fromIntegral resolution
                                                         )
                                                       , ( fromIntegral x
                                                         , fromIntegral y + fromIntegral resolution / 2
                                                         )
                                                       ]
isolines (Full , Full , Empty, Full ) (x, y) = polygon [ ( fromIntegral x
                                                         , fromIntegral y
                                                         )
                                                       , ( fromIntegral x + fromIntegral resolution
                                                         , fromIntegral y
                                                         )
                                                       , ( fromIntegral x + fromIntegral resolution
                                                         , fromIntegral y + fromIntegral resolution / 2
                                                         )
                                                       , ( fromIntegral x + fromIntegral resolution / 2
                                                         , fromIntegral y + fromIntegral resolution
                                                         )
                                                       , ( fromIntegral x
                                                         , fromIntegral y + fromIntegral resolution
                                                         )
                                                       ]
isolines (Full , Empty, Full , Full ) (x, y) = polygon [ ( fromIntegral x
                                                         , fromIntegral y
                                                         )
                                                       , ( fromIntegral x + fromIntegral resolution / 2
                                                         , fromIntegral y
                                                         )
                                                       , ( fromIntegral x + fromIntegral resolution
                                                         , fromIntegral y + fromIntegral resolution / 2
                                                         )
                                                       , ( fromIntegral x + fromIntegral resolution
                                                         , fromIntegral y + fromIntegral resolution
                                                         )
                                                       , ( fromIntegral x
                                                         , fromIntegral y + fromIntegral resolution
                                                         )
                                                       ]
isolines (Empty, Full , Full , Full ) (x, y) = polygon [ ( fromIntegral x + fromIntegral resolution / 2
                                                         , fromIntegral y
                                                         )
                                                       , ( fromIntegral x + fromIntegral resolution
                                                         , fromIntegral y
                                                         )
                                                       , ( fromIntegral x + fromIntegral resolution
                                                         , fromIntegral y + fromIntegral resolution
                                                         )
                                                       , ( fromIntegral x
                                                         , fromIntegral y + fromIntegral resolution
                                                         )
                                                       , ( fromIntegral x
                                                         , fromIntegral y + fromIntegral resolution / 2
                                                         )
                                                       ]
isolines (Full , Full , Full , Full ) (x, y) = polygon [ ( fromIntegral x
                                                         , fromIntegral y
                                                         )
                                                       , ( fromIntegral x + fromIntegral resolution
                                                         , fromIntegral y
                                                         )
                                                       , ( fromIntegral x + fromIntegral resolution
                                                         , fromIntegral y + fromIntegral resolution
                                                         )
                                                       , ( fromIntegral x
                                                         , fromIntegral y + fromIntegral resolution
                                                         )
                                                       ]
