module Chess where

import InputModel exposing (..)
import GameModel exposing (GameState, defaultGame)
import GameManager exposing (checkValidGame)

import Signal exposing (..)
import Rendering exposing (display)
import Time exposing (every, second)
import Date exposing (year, hour, minute, second, fromTime)


-- These ports are directly connected to JavaScript, they are incoming ports. Their counterpart is in chess.html.
-- Semantically, they should be declared on InputModel, but elm requires ports to be declared on main elm file.
port newGameButton: Signal Bool
port gameTypeChangedButton: Signal Bool
port currentTimestamp: Signal Int
port mayhemActivated: Signal Bool

-- These are the actions that will trigger a new gameState.
actions: Signal Update
actions = mergeMany
            [ map Move playerDirection,
            map NewGameButtonPressed newGameButton,
            map GameTypeChanged gameTypeChangedButton,
            map EnterPressed enterPressed,
            map MayhemActivated mayhemActivated,
            map Tick clock
            ]

input = Input <~ actions ~ currentTimestamp

-- Folds the input into the gameState, starting with the defaultGame.
gameState: Signal GameState
gameState = foldp checkValidGame defaultGame input

-- Display the game.
main = display <~ gameState

-- The clock is used as an action because when mayhem is activated, we want computer to play against itself without the need
-- of making a move, so we need to update gameState. This isn't the best solution, as an event is trigger every second; In a future implementation, it would be better to trigger the clock depending on GameTypeChanged.
clock: Signal Float
clock = Time.every Time.second