module Game where

-- import           Debug.Trace
import           Data.Array
import           Graphics.Gloss.Interface.Pure.Game

data Cell  = Full | Empty deriving (Eq, Show)
type Board = Array (Int, Int) Cell
type World = Board

screenWidth :: Int
screenWidth = 800

screenHeight :: Int
screenHeight = 600

resolution :: Int
resolution = 25

initialState :: World
initialState = array indexRange $ zip (range indexRange) (repeat Empty)
  where indexRange = ( (0, 0)
                     , ( ceiling (fromIntegral screenWidth  / fromIntegral resolution :: Float)
                       , ceiling (fromIntegral screenHeight / fromIntegral resolution :: Float)
                       )
                     )

mousePosAsCellCoord :: (Float, Float) -> (Int, Int)
mousePosAsCellCoord (x, y) = ( round $ remap x (- fromIntegral screenWidth / 2 , fromIntegral screenWidth / 2 )
                                               (0.0, fromIntegral screenWidth / fromIntegral resolution )
                             , round $ remap y (- fromIntegral screenHeight / 2, fromIntegral screenHeight / 2)
                                               (0.0, fromIntegral screenHeight / fromIntegral resolution)
                             )
  where remap a (omin, omax) (nmin, nmax) = (((a - omin) * (nmax - nmin)) / (omax - omin)) + nmin

switchCell :: World -> (Int, Int) -> World
switchCell world pos =
  case world ! pos of
    Empty -> world // [(pos, Full)]
    Full  -> world // [(pos, Empty)]

eventHandler :: Event -> World -> World
eventHandler (EventKey (MouseButton LeftButton) Up _ pos) world = switchCell world $ mousePosAsCellCoord pos
-- eventHandler (EventKey (MouseButton LeftButton) Up _ pos) world =
--     trace ("mouse pos = " ++ show pos) (switchCell world $ mousePosAsCellCoord pos)
eventHandler _ world = world

update :: Float -> World -> World
update = const id
