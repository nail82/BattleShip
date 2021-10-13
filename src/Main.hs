module Main where

import Data.Char (toLower)
import System.Exit (exitSuccess)
import Control.Monad

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
renderBoard :: Bool -- ^ False turns off ship rendering
            -> Hits -- ^ The list of hit coordinates
            -> Misses -- ^ The list of miss coordinates
            -> Ships -- ^ The list of ship coordinates
            -> String
renderBoard = undefined

-- Render the board with ships hidden
hiddenShips :: Hits -> Misses -> Ships -> String
hiddenShips = renderBoard True

-- Render the ships after the game ends
postGameBoard :: Hits -> Misses -> Ships -> String
postGameBoard = renderBoard False


splash :: String
splash = "\n\nWelcome to Haskell BattleShip!\n\nCommand options are (n)ew or (q)uit"

play :: IO ()
play = putStrLn "Playing..."

quit :: IO ()
quit = do
  putStrLn "\n\nThanks for playing =)"
  exitSuccess


main :: IO ()
main = forever $ do
         putStrLn splash
         cmd@(c:_) <- getLine
         if 'n' == (toLower c) then play
         else if 'q' == (toLower c) then quit
         else putStrLn $ cmd <> " isn't a valid command."
