module Main where

import Data.List

{--
  Here we have an implementation of the game BattleShip in Haskell.
--}

-- A representation of a Cartesian coordinate
data Coord = Coord (Int, Int)
           deriving (Eq, Show, Ord)

-- We don't maintain the entire board, just lists of
-- coordinate pairs that have meaning in the game.
type Hits = [Coord]
type Misses = [Coord]
type Ships = [Coord]

-- |A constant for the header
boardHeader :: String
boardHeader = " 0 1 2 3 4 5 6 7 8 9"

-- Draw the board
renderBoard :: Bool -> Hits -> Misses -> Ships -> String
renderBoard = undefined

-- Render the board with ships hidden
hiddenShips :: Hits -> Misses -> Ships -> String
hiddenShips = renderBoard True

-- Render the ships after the game ends
postGameBoard :: Hits -> Misses -> Ships -> String
postGameBoard = renderBoard False


main :: IO ()
main = do
  let coord = Coord (1, 2)
      coords = [coord, Coord (3, 4)]
  putStrLn $ intercalate "\n" $ show <$> coords
