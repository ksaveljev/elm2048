module Elm2048 where

import InputModel (Input, Controls, playerDirection, randomFloats)
import GameModel (defaultGame, GameState)
import Logic (stepGame)
import Rendering (display)

port score : Signal Int -- Outgoing score port
port score = (\x -> x.score) <~ gameState

port newGameButton : Signal Bool -- Incoming new game button port

controls = Controls <~ playerDirection ~ newGameButton
input = Input <~ controls ~ (randomFloats controls)

-- fold the input into the game state, starting with the default game state
gameState : Signal GameState
gameState = foldp stepGame defaultGame input

main = display <~ gameState
