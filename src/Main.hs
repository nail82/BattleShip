module Main where

import Data.List (intercalate, intersperse, elem, delete)
import Data.Char (toLower)
import System.Exit (exitSuccess)
import Control.Monad

{--
  Here we have an implementation of the game BattleShip in Haskell.
--}

-- -------------------
-- Type definitions
-- -------------------
type Coord = (Int, Int)
type Hits = [Coord]
type Misses = [Coord]
type Ships = [Coord]
-- |Board is the tuple of ships, hits and misses
-- |We don't maintain the entire board, just lists of
-- |coordinate pairs that have meaning in the game.
type Board = (Ships, Hits, Misses)

-- ------------------
-- Board rendering
-- ------------------

-- The board header row
boardHeader :: String
boardHeader = "  0 1 2 3 4 5 6 7 8 9"

renderBoard :: Bool -- ^ False turns off ship rendering
            -> Board -- ^ Ships, Hits, Misses
            -> String
renderBoard showShips board =
    -- Break the board apart into its components
    let (ships, hits, misses) = board
        -- Coordinates of the board as a list (ie [(0,0), (0,1), (0,2)...]).
        boardCoords = [(x,y) | x <- [0..9], y <- [0..9]]
        -- Map boardMarker across the coordinates to produce the board as a one-dimensional string.
        markers     = fmap boardMarker boardCoords
        -- Put a space between markers.
        markers'    = intersperse ' ' markers
        -- Append the header and convert the board to two dimensions.
        rows'       = [boardHeader] <> markersToRows markers' [] 0
        -- Put a newline between rows
        two_D_board = intercalate "\n" rows'

        -- Helper functions
        -- |Compute the marker for a coordinate
        boardMarker coord
            | (elem coord ships) && showShips = 's'
            | (elem coord ships) && (not showShips) = '_'
            | elem coord hits = 'x'
            | elem coord misses = 'o'
            | otherwise = '_'

        -- |A recursive function to split the single row of markers
        -- |into a list of rows appending the row number to each row.
        markersToRows :: String -> [String] -> Int -> [String]
        markersToRows "" rows _ = reverse rows
        markersToRows ms rows n =
            let (r,ms') = splitAt 20 ms
                rowNum = (show n) <> " "
            in markersToRows ms' (rowNum <> r:rows) (n+1)

    in two_D_board

-- |Draw the board with ships hidden
hiddenShips :: Board -> String
hiddenShips = renderBoard False

-- |Draw the board with ships after the game ends
postGameBoard :: Board -> String
postGameBoard = renderBoard True

-- --------------------
-- End Board rendering
-- --------------------

-- --------------------
-- IO Handling
-- --------------------
splash :: String
splash = "\n\nWelcome to Haskell BattleShip!\n\nCommand options are (n)ew game or (q)uit"

sillyIntParser :: String -> Maybe Int
sillyIntParser s
    | s == "0" = Just 0
    | s == "1" = Just 1
    | s == "2" = Just 2
    | s == "3" = Just 3
    | s == "4" = Just 4
    | s == "5" = Just 5
    | s == "6" = Just 6
    | s == "7" = Just 7
    | s == "8" = Just 8
    | s == "9" = Just 9
    | otherwise = Nothing

boundsCheck :: String -> String -> Maybe Coord
boundsCheck x y =
    let xys = sequence $ fmap sillyIntParser [x,y]
    in case xys of
         Just (x':y':_) -> Just (x',y')
         _ -> Nothing

stringToCoords :: String -> Maybe Coord
stringToCoords coordStr =
    case words coordStr of
      [] -> Nothing
      (_:[]) -> Nothing
      (x:y:_) -> boundsCheck x y

-- --------------------
-- End IO Handling
-- --------------------

-- --------------------
-- Game play
-- --------------------

-- Get a coordinate pair from the user.
getCoord :: String -> Int -> IO Coord
getCoord s _ = do
  putStrLn ("Enter " <> s <> " coordinates (separated by whitespace) => ")
  coord_str <- getLine
  let maybe_coord = stringToCoords coord_str
  case maybe_coord of
    Just c -> return c
    Nothing -> do
        putStrLn "Hmmm, that didn't look right.  Try again.\n"
        getCoord s 0


setShips :: IO Board
setShips = do
  putStrLn "How many ships (1-9)?"
  n <- getLine
  let maybe_n = sillyIntParser n
  case maybe_n of
    Just n' -> do
             coords <- forM [1..n'] (getCoord "ship")
             return (coords, [], [])
    Nothing -> do
             putStrLn "Hmmm, I need a number from 1 to 9.  Try again.\n"
             setShips


takeAShot :: Board -> IO Board
takeAShot board = do
  putStrLn $ hiddenShips board
  coord <- getCoord "shot" 0
  let (ships, hits, misses) = board
  let aHit = coord `elem` ships
  let board' | aHit =
                 (delete coord ships, (coord:hits), misses)
             | otherwise =
                 (ships, hits, (coord:misses))
  putStrLn $ hiddenShips board'
  if aHit then putStrLn "A fine hit!" else putStrLn "Missed. derp."
  return board'


runGame :: Board -> IO ()
runGame board = do
  let (ships, _, _) = board
  if length ships == 0 then
      do
        putStrLn $ postGameBoard board
        putStrLn "You won!"
        exitSuccess
  else do
    board' <- takeAShot board
    runGame board'


play :: IO ()
play = do
  board <- setShips
  runGame board



-- --------------------
-- End Game play
-- --------------------

quit :: IO ()
quit = do
  putStrLn "\n\nLeaving so soon?\n"
  exitSuccess


main :: IO ()
main = forever $ do
         putStrLn splash
         cmdStr <- getLine

         -- Handle user input
         let go [] = putStrLn "Oops, need a command."
             go (c:_)
                 | (toLower c) == 'n' = play
                 | (toLower c) == 'q' = quit
                 | otherwise = putStrLn $ cmdStr <> " isn't a valid command."

         go cmdStr
