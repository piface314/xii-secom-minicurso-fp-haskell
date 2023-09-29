module TicTacToe (run) where

import           Data.List                            (find, foldl', foldl1')
import           Data.Matrix                          (Matrix)
import qualified Data.Matrix                          as Matrix
import           Data.Maybe
import qualified Data.Vector                          as Vector
import           Graphics.Gloss
import           Graphics.Gloss.Interface.IO.Interact


data Player = PlayerO | PlayerX deriving (Eq, Show)
data Move = Move Player (Int,Int) deriving (Eq, Show)
type Grid = Matrix (Maybe Player)
data WinLine = Row Int | Col Int | DiagMain | DiagAnti
type Winner = (Player, WinLine)
data Game = Game { grid :: Grid, moves :: [Move], winner :: Maybe Winner }


cellN :: Int
cellN = 3

cellSize :: Float
cellSize = 200

run :: IO ()
run = do
    let w = round cellSize * cellN + 50
    let h = round cellSize * cellN + 50
    let window = InWindow "Tic-Tac-Toe" (w, h) (100, 100)
    play window black 30 initialState draw onEvent tick

tick :: Float -> Game -> Game
tick = const id

initialState :: Game
initialState = Game { grid = initialGrid, moves = [], winner = Nothing }

initialGrid :: Grid
initialGrid = Matrix.matrix cellN cellN (const Nothing)

doMove :: Move -> Grid -> Grid
doMove (Move player pos@(i,j)) = Matrix.setElem (Just player) pos

checkMove :: Move -> Grid -> Bool
checkMove (Move player (i,j)) = isNothing . Matrix.getElem i j

checkWinner :: Grid -> Maybe Winner
checkWinner grid =
    case catMaybes (rows ++ cols ++ diagM ++ diagA) of
        (winner:_) -> Just winner
        []         -> Nothing
    where
        ns = [1..cellN]
        rows = zipWith check (map Row ns) $ map ((`zip` ns) . repeat) ns
        cols = zipWith check (map Col ns) $ map ((ns `zip`) . repeat) ns
        diagM = [check DiagMain $ zip ns ns]
        diagA = [check DiagAnti $ zip ns (reverse ns)]

        check :: WinLine -> [(Int,Int)] -> Maybe Winner
        check wl pos = do
            player <- foldl1' check' $ map (flip (uncurry Matrix.getElem) grid) pos
            Just (player, wl)

        check' :: Maybe Player -> Maybe Player -> Maybe Player
        check' (Just PlayerO) (Just PlayerO) = Just PlayerO
        check' (Just PlayerX) (Just PlayerX) = Just PlayerX
        check' _ _                           = Nothing

nextPlayer :: Game -> Player
nextPlayer game
    | null m = PlayerO
    | player == PlayerO = PlayerX
    | player == PlayerX = PlayerO
    where
        m = moves game
        (Move player _) = head m

draw :: Game -> Picture
draw game = Pictures (drawGridLines game ++ drawMarks game ++ [drawWinLine game])

drawGridLines :: Game -> [Picture]
drawGridLines game = map (Color white) (hLines ++ vLines)
    where
        linePos = take (cellN - 1) $ drop 1 thresholds
        lineLength = cellSize * fromIntegral cellN
        hLines = map (\x -> Translate x 0 $ rectangleSolid thickness lineLength) linePos
        vLines = map (\y -> Translate 0 y $ rectangleSolid lineLength thickness) linePos

drawMarks :: Game -> [Picture]
drawMarks game = Matrix.toList $ Matrix.mapPos drawMark (grid game)
    where
        drawMark :: (Int,Int) -> Maybe Player -> Picture
        drawMark (i,j) mark = Translate (indexToPos j) (indexToPos i) $ drawMark' mark

        drawMark' :: Maybe Player -> Picture
        drawMark' (Just PlayerX) =
            Color green
            $ Pictures
            $ zipWith Rotate [-45,45]
            $ replicate 2
            $ rectangleSolid (thickness*2) (cellSize - 20)
        drawMark' (Just PlayerO) =
            Color blue
            $ ThickCircle (cellSize / 2 - thickness * 4) (thickness * 2)
        drawMark' Nothing = Blank

drawWinLine :: Game -> Picture
drawWinLine game = fromMaybe Blank winLine
    where winLine = do
            (player, pos) <- winner game
            let c = if player == PlayerO then makeColor 0 0 1 0.5 else makeColor 0 1 0 0.5
            let l = cellSize * fromIntegral cellN
            Just $ Color c $
                case pos of
                Row i -> Translate 0 (indexToPos i) $ rectangleSolid (l - 20) (thickness*2)
                Col j -> Translate (indexToPos j) 0 $ rectangleSolid (thickness*2) (l - 20)
                DiagMain -> Rotate (-45) $ rectangleSolid (l * sqrt 2 - 20) (thickness*2)
                DiagAnti -> Rotate 45 $ rectangleSolid (l * sqrt 2 - 20) (thickness*2)

onEvent :: Event -> Game -> Game
onEvent (EventKey (MouseButton LeftButton) Up _ pos) game
    | isJust (winner game) = game
    | otherwise = fromMaybe game $ do
        move <- moveFromEvent game pos
        grid' <- doSafeMove move
        Just Game { grid = grid', moves = move : moves game, winner = checkWinner grid' }
    where
        doSafeMove move =
            if checkMove move (grid game)
                then Just $ doMove move (grid game)
                else Nothing
onEvent (EventKey (Char 'z') Up _ pos) game = Game { grid = grid', moves = moves', winner = Nothing }
    where
        moves' = drop 1 (moves game)
        grid' = currentGrid moves'
onEvent _ game = game

currentGrid :: [Move] -> Grid
currentGrid = foldr doMove initialGrid

moveFromEvent :: Game -> (Float,Float) -> Maybe Move
moveFromEvent game (x,y) = do
        (_, i) <- find (inRange y . fst) ts
        (_, j) <- find (inRange x . fst) ts
        Just $ Move (nextPlayer game) (i, j)
    where
        ts = zip (pairs thresholds) [1..]
        inRange x (a,b) = a <= x && x < b

thresholds :: [Float]
thresholds = map (\i -> (fromIntegral i - fromIntegral cellN / 2) * cellSize) [0..cellN]

indexToPos :: Int -> Float
indexToPos i = (fromIntegral i - fromIntegral cellN / 2 - 0.5) * cellSize

thickness :: Float
thickness = 10

pairs :: [a] -> [(a,a)]
pairs []       = []
pairs [_]      = []
pairs (a:b:xs) = (a,b) : pairs (b:xs)
