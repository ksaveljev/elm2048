module Logic where

import InputModel (
    Input
  , Direction
  , Up
  , Down
  , Left
  , Right
  , None
)

import GameMode (
    GameState
  , Tile
  , Number
  , Empty
  , defaultGame
  , emptyGrid
  , gridSize
  , Grid
  , setTile
  , readTile
  , tileToInt
  , intToTile
  , tilesWithCoordinates
  , rotateGrid
  , InProgress
  , GameOver
  , Won
)

-- takes a list of values and 'slides' them to the left,
-- joining in lists pairs of adjacent identical values
groupedByTwo : [a] -> [[a]]
groupedByTwo l = case l of
                   [x] -> [[x]]
                   [x,y] -> if (x == y) then [[x, y]] else [[x], [y]]
                   (x::y::xs) -> if (x == y)
                                    then ([x, y] :: (groupedByTwo xs))
                                    else ([x] :: (groupedByTwo (y :: xs)))
                   otherwise -> []

-- slides list of tiles to left, merging tiles where necessary,
-- and returning a full list of four tiles, and the number of points gained
slideRow : [Tile] -> ([Tile], Int)
slideRow r =
  let grouped = groupedByTwo <| filter (\t -> t /= Empty) r
  in (
       take gridSize
       <| map (intToTile . sum . (map tileToInt)) grouped) ++ repeat gridSize Empty
     , sum . (map tileToInt) <| concat <| filter (\x -> length x > 1) grouped
     )


slideGrid : Direction -> Grid -> (Grid, Int)
slideGrid dir grid =
  let rotatedGrid = (case dir of
                       Down -> rotateGrid
                       Right -> rotateGrid . rotateGrid
                       Up -> rotateGrid . rotateGrid . rotateGrid
                       otherwise -> id)
                    <| grid

      rowsWithScores = map slideRow <| (\(Grid h) -> h) <| rotateGrid

      slidRotatedGrid = Grid <| map fst rowsWithScores

      scoreGained = sum <| map snd rowsWithScores

      slidGrid = (case dir of
                    Up -> rotateGrid
                    Right -> rotateGrid . rotateGrid
                    Down -> rotateGrid . rotateGrid . rotateGrid
                    otherwise -> id)
                 <| slidRotatedGrid
  in (slidGrid, scoreGained)

-- push the tiles in the grid according to the direction in the input
slideGameState : Input -> GameState -> GameState
slideGameState input gameState =
  let newGridScore = slideGrid input.controls.tilePushDirection gameState.grid
  in if fst newGridScore == gameState.grid 
        then gameState
        else { gameState |
                 grid <- fst newGridScore
               , score <- gameState.score + snd newGridScore
             }

-- checks if a 2048 tile is present in the grid
gameWon : Grid -> Bool
gameWon (Grid g) = 0 /= (length <| filter (\t -> t == Number 2048) <| concat g)

-- check if none of the rows or columns of a grid can be slid in any direction
gameLost : Grid -> Bool
gameLost g =
  let up = fst <| slideGrid Up g
      down = fst <| slideGrid Down g
      left = fst <| slideGrid Left g
      right = fst <| slideGrid Right g
  in and [ g /= emptyGrid, up == down, down == left, left == right, right == g ]

win : GameState -> GameState
win gameState = { gameState | gameProgress <- Won }

lose : GameState -> GameState
lose gameState = { gameState | gameProgress <- GameOver }

tile2Probability : Float
tile2Probability = 0.9

newTile : Float -> Tile
newTile x = if x < tile2Probability then Number 2 else Number 4

emptyTiles : Grid -> [(Int, Int)]
emptyTiles g = map (\(_, i, j) -> (i, j))
                   <| filter (\(t, _, _) -> t == Empty)
                   <| tilesWithCoordinates g

newTileIndex : Float -> Grid -> Maybe (Int, Int)
newTileIndex x g =
  let emptyTileIndices = emptyTiles g
  in case emptyTileIndices of
       [] -> Nothing
       otherwise -> Just <| emptyTileIndices ! (floor <| (toFloat <| length emptyTileIndices) * x)

placeRandomTile : Float -> Float -> GameState -> GameState
placeRandomTile float1 float2 gameState =
  let tileIndex = newTileIndex float1 gameState.grid
  in if tileIndex == Nothing
        then gameState
        else { gameState |
                grid <- setTile (maybe (0, 0) id <| tileIndex) gameState.grid <| newTile float2
             }

newGame : Input -> GameState
newGame input = placeRandomTile (input.randomFloats ! 0) (input.randomFloats ! 1)
             <| placeRandomTile (input.randomFloats ! 2) (input.randomFloats ! 3)
             <| defaultGame

stepGame : Input -> GameState -> GameState
stepGame input gameState =
  if | input.controls.newGameButtonPressed -> newGame input
     | gameState.gameProgress /= InProgress -> gameState
     | gameWon gameState.grid -> win gameState
     | gameLost gameState.grid -> lose gameState
     | input.controls.tilePushDirection /= None ->
        let pushedState = slideGameState input gameState
        in if pushedState == gameState
              then gameState
              else placeRandomTile (input.randomFloats ! 0) (input.randomFloats ! 1) pushedState
     | otherwise -> gameState
