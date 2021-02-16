module Main where

import           Graphics.Gloss

import           Game
import           Rendering

window :: Display
window = InWindow "Marching Squares" (screenWidth, screenHeight) (100, 100)

backgroundColor :: Color
backgroundColor = makeColor 0.5 0.5 0.5 1.0

main :: IO ()
main = play window backgroundColor 30 initialState render eventHandler update
