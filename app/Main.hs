module Main where

import qualified GameOfLife as Life
import           System.IO  (IOMode (..), openFile)
import qualified TicTacToe  as TTT
import System.Environment (getArgs)

main :: IO ()
main = do
    (game:args) <- getArgs
    case game of
        "life" -> Life.run (head args)
        "tic-tac-toe" -> TTT.run
        _ -> error "Unknown game"
