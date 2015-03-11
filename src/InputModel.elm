module InputModel where

import Signal exposing (..)
import Keyboard

type Direction = Up | Down | Left | Right | None

type Update = Move Direction | NewGameButtonPressed Bool | GameTypeChanged Bool | CurrentTimestampChanged Int | EnterPressed Bool | MayhemActivated Bool | Tick Float

type alias Input = { -- inputs that the game will depend of
    action: Update, -- the user actions
    currentTimestamp: Int -- the current time
}

-- Signal which represents the direction that the user has chosen.
-- Compatible with both the wasd and arrow keys.
playerDirection: Signal Direction
playerDirection = let toDirection ds =
                      if | ds == {x = 0, y = 1} -> Up
                         | ds == {x = 0, y = -1} -> Down
                         | ds == {x = 1, y = 0} -> Right
                         | ds == {x = -1, y = 0} -> Left
                         | otherwise -> None
                      in merge (toDirection <~ Keyboard.arrows) (toDirection <~ Keyboard.wasd)

enterPressed: Signal Bool
enterPressed = Keyboard.enter
