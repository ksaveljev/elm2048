module Rendering where

import GameModel (
    Tile(..)
  , Grid
  , gridSize
  , tilesWithCoordinates
  , GameState
  , Progress(..)
  )

import Text(
    Style
  , style
  )

tileSize : Float -- the width of a tile
tileSize = 106.25

tileMargin : Float -- the width of the gaps between tiles
tileMargin = 15

tileColor : Tile -> Color
tileColor tile = case tile of
                   Number 2 -> rgb 238 228 218
                   Number 4 -> rgb 237 224 200
                   Number 8 -> rgb 242 177 121
                   Number 16 -> rgb 245 149 99
                   Number 32 -> rgb 246 124 95
                   Number 64 -> rgb 246 94 59
                   Number 128 -> rgb 237 207 114
                   Number 256 -> rgb 237 204 97
                   Number 512 -> rgb 237 200 80
                   Number 1024 -> rgb 237 197 63
                   Number 2048 -> rgb 237 194 46
                   otherwise -> rgba 238 228 218 0.35 -- empty tile

tileTextColor : Tile -> Color
tileTextColor tile = case tile of
                       Number n -> if n >= 8 then rgb 249 246 242 else rgb 119 110 101
                       otherwise -> black -- empty tile

tileTextSize : Tile -> Float
tileTextSize tile = case tile of
                      Number 128 -> 45
                      Number 256 -> 45
                      Number 512 -> 45
                      Number 1024 -> 35
                      Number 2048 -> 35
                      otherwise -> 55

tileTextStyle : Tile -> Style
tileTextStyle tile = {
    typeface = [ "Helvetica Neue", "Arial", "sans-serif" ]
  , height = Just <| tileTextSize tile
  , color = tileTextColor tile
  , bold = True
  , italic = False
  , line = Nothing
  }

displayTile : Tile -> Element
displayTile tile =
  let tileBackground = filled (tileColor tile) <| square tileSize
  in case tile of
       Number n -> collage (round tileSize) (round tileSize)
                     [
                       tileBackground
                     , toForm <| centered
                              <| style (tileTextStyle tile)
                              <| toText
                              <| show n
                     ]
       Empty -> collage (round tileSize) (round tileSize) [ tileBackground ]

displayTileAtCoordinates : (Tile, Int, Int) -> Form
displayTileAtCoordinates (t, i, j) =
  let position = (
                   (tileSize + tileMargin) * (toFloat i - (toFloat gridSize - 1) / 2)
                 , (-1) * (tileSize + tileMargin) * (toFloat j - (toFloat gridSize - 1) / 2)
                 )
  in move position <| toForm <| displayTile t

gridWidth : Float
gridWidth = (toFloat gridSize) * tileSize + (1 + toFloat gridSize) * tileMargin

displayGrid : Grid -> Element
displayGrid g =
  let gridBox = filled (rgb 187 173 160) <| square gridWidth
      tiles = map displayTileAtCoordinates <| tilesWithCoordinates g
  in collage (round gridWidth) (round gridWidth) ([gridBox] ++ tiles)

displayOverlay : Style -> Color -> String -> Element
displayOverlay s c t = collage (round gridWidth) (round gridWidth)
                         [
                           filled c <| square gridWidth
                         , toForm <| centered <| style s <| toText t
                         ]

gameOverOverlayStyle : Style
gameOverOverlayStyle = tileTextStyle <| Number 2

wonOverlayStyle : Style
wonOverlayStyle = tileTextStyle <| Number 16

gameOverOverlayColor : Color
gameOverOverlayColor = rgba 238 228 218 0.73

wonOverlayColor : Color
wonOverlayColor = rgba 237 194 46 0.5

gameOverMessage : String
gameOverMessage = "Game over!"

wonMessage : String
wonMessage = "You won!"

displayGameOverOverlay : Element
displayGameOverOverlay = displayOverlay
                           gameOverOverlayStyle
                           gameOverOverlayColor
                           gameOverMessage

displayWonOverlay : Element
displayWonOverlay = displayOverlay
                      wonOverlayStyle
                      wonOverlayColor
                      wonMessage

applyOverlay : Element -> Element -> Element
applyOverlay overlay grid = collage (round gridWidth) (round gridWidth) [toForm <| grid, toForm <| overlay]

display : GameState -> Element
display gameState = (case gameState.gameProgress of
                       GameOver -> applyOverlay displayGameOverOverlay
                       Won -> applyOverlay displayWonOverlay
                       otherwise -> identity)
                    <| displayGrid gameState.grid
