module InputModel where

import Keyboard
import Random

data Direction = Up | Down | Left | Right | None -- the direction to shift the grid

type Controls = { -- define the user controls
    tilePushDirection : Direction -- the direction the user wants to slide the grid
  , newGameButtonPressed : Bool   -- whether the new game button is pressed
}

type Input = { -- define the inputs that the game will depend upon
    controls : Controls    -- the user controls
  , randomFloats : [Float] -- a source of randomness

-- make a signal that is the direction that the user has chosen
-- compatible with both the wasd and arrow keys
playerDirection : Signal Direction
playerDirection =
  let toDirection ds =
    if | ds == {x = 0, y = 1} -> Up
       | ds == {x = 0, y = -1} -> Down
       | ds == {x = 1, y = 0} -> Right
       | ds == {x = -1, y = 0} -> Left
       | otherwise -> None
  in merge (toDirection <~ Keyboard.arrows) (toDirection <~ Keyboard.wasd)

-- provide four random floats that will be used for random events in the game
-- logic; changes every time the signal s changes
randomFloats : Signal a -> Signal [Float]
randomFloats s = Random.floatList <| sampleOn s <| constant 4
