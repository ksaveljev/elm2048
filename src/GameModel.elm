module GameModel where

import Utils ((!), transpose)

data Tile = Number Int | Empty

data Grid = Grid [[Tile]]

data Progress = InProgress | GameOver | Won

type GameState = {
    grid : Grid
  , score : Int
  , gameProgress : Progress
}

gridSize : Int -- the length of the sides of the grid
gridSize = 4

readTile : (Int, Int) -> Grid -> Tile
readTile (i, j) (Grid g) = (g ! j) ! i

setTile : (Int, Int) -> Grid -> Tile -> Grid
setTile (i, j) (Grid g) t =
  let r = g ! j
      nr = (take i r) ++ [t] ++ (drop (i+1) r)
  in Grid <| (take j g) ++ [nr] ++ (drop (j+1) g)

tileToInt : Tile -> Int
tileToInt t = case t of
                Number n -> n
                otherwise -> 0

intToTile : Int -> Tile
intToTile n = case n of
                0 -> Empty
                otherwise -> Number n

tilesWithCoordinates : Grid -> [(Tile, Int, Int)]
tilesWithCoordinates (Grid g) = concat
                             <| zipWith (\j r -> map (\(t, i) -> (t, i, j)) r) [0..(gridSize-1)]
                             <| map (\r -> zip r [0..(gridSize-1)])
                             <| g

rotateGrid : Grid -> Grid -- rotate a grid clockwise by 90 degrees
rotateGrid (Grid g) = Grid <| map reverse <| transpose g

emptyGrid : Grid
emptyGrid = Grid <| repeat gridSize <| repeat gridSize <| Empty

defaultGame : GameState
defaultGame = {
    grid = emptyGrid
  , score = 0
  , gameProgress = InProgress
  }
