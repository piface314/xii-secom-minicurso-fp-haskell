module GameOfLife (initialState, run) where

import           Data.Matrix    (Matrix)
import qualified Data.Matrix    as Matrix
import           Data.Maybe     (fromMaybe)
import           Graphics.Gloss

type Grid = Matrix Bool


cellSize :: Int
cellSize = 10

initialState :: String -> IO Grid
initialState fp = do
    contents <- readFile fp
    return $ Matrix.fromLists (map (map (== '#')) (lines contents))

run :: String -> IO ()
run fp = do
    grid <- initialState fp
    let w = Matrix.ncols grid
    let h = Matrix.nrows grid
    let evolve _ _ = next
    simulate (InWindow "Game of Life" ((w+1)*cellSize, (h+1)*cellSize) (0, 0)) black 12 grid toPicture evolve

next :: Grid -> Grid
next grid = Matrix.mapPos (next' grid) grid

next' :: Grid -> (Int, Int) -> Bool -> Bool
next' grid (i,j) v = (v && alive == 2) || alive == 3
    where
        moves = [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)]
        neighbours = map (\(di,dj) -> Matrix.safeGet (i+di) (j+dj) grid) moves
        alive = length $ filter (fromMaybe False) neighbours

toPicture :: Grid -> Picture
toPicture grid =
    Pictures [
        Color white (rectangleWire w' h'),
        Scale 1 (-1) $
            Translate (-w'/2.0) (-h'/2.0) $
                Pictures (map Pictures (Matrix.toLists cells))
    ]
    where
        w' = fromIntegral $ Matrix.ncols grid * cellSize
        h' = fromIntegral $ Matrix.nrows grid * cellSize
        cells = Matrix.mapPos toPicture' grid

toPicture' :: (Int, Int) -> Bool -> Picture
toPicture' (i,j) v =
    Translate x y $
        Color (makeColor 1 1 1 (if v then 1 else 0)) $
            rectangleSolid (fromIntegral cellSize) (fromIntegral cellSize)
    where
        x = (fromIntegral j - 0.5) * fromIntegral cellSize
        y = (fromIntegral i - 0.5) * fromIntegral cellSize
