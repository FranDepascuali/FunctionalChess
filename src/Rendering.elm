module Rendering where

import Utils exposing (..)
import GameModel exposing (..)
import Tile exposing (..)
import Board exposing (..)
import Logic exposing (..)
import Piece exposing (..)

import Color exposing (..)
import Text exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import List exposing (..)

tileSize: Float
tileSize = 70

tileMargin: Float
tileMargin = 8

darkgreen = rgb 77 153 60

tileColor: Tile -> Utils.Position -> GameState -> Color
tileColor tile position gameState =
        if  | gameState.mayhem == True
                -> displayNormalTile position
            | position == gameState.cursorAt && (gameState.turn == White || gameState.gameType /= OneVSComputer)
                -> red
            | gameState.selected /= Nothing
                -> case gameState.selected of
                    Just tileSelected -> if canMoveTile gameState.board gameState.turn (readTile tileSelected gameState.board) tileSelected position then blue else displayNormalTile position
            | otherwise
                -> displayNormalTile position

canMoveTile: Board -> PieceColor -> Tile -> Utils.Position -> Utils.Position -> Bool
canMoveTile board color tile from to = mapWithDefault (\piece -> canMakeMove board color piece from to) tile False

displayNormalTile: Utils.Position -> Color
displayNormalTile (x,y) = case (isEven x, isEven y) of
                            (True, True) -> lightBrown
                            (False, False) -> lightBrown
                            otherwise -> darkBrown

wonTextColor: Tile -> Color -- the text color of a tile
wonTextColor tile = black

wonTextSize: Tile -> Float -- the text size of a tile
wonTextSize tile = 50

wonTextStyle: Tile -> Style -- the text style of a tile
wonTextStyle tile = {
                  typeface = [ "Helvetica Neue", "Arial", "sans-serif" ]
                , height = Just <| wonTextSize tile
                , color = wonTextColor tile
                , bold = True
                , italic = False
                , line = Nothing
                }

displayTile: Tile -> Utils.Position -> GameState -> Element -- display a tile
displayTile tile position gameState =
        let tileBackground = filled (tileColor tile position gameState)
        <| square tileSize
                in case tile of
                    Just (Piece c t) -> collage (round tileSize) (round tileSize)
                        [
                            tileBackground,
                            toForm (image 75 75 ("images/" ++ toString c ++ "_" ++ toString t ++ ".jpg"))
                        ]
                    Nothing -> collage (round tileSize) (round tileSize)
                       [
                            tileBackground
                        ]

displayTileAtCoordinates: (Tile, Int, Int) -> GameState -> Form
displayTileAtCoordinates (t,i,j) gameState =
                        let
                            p = ((tileSize + tileMargin) * (toFloat j - (toFloat boardSize - 1)/2),
                             (-1) * (tileSize + tileMargin) * (toFloat i - (toFloat boardSize - 1)/2))
                        in
                            move p <| toForm <| displayTile t (i,j) gameState

boardWidth: Float -- the width of the entire game grid
boardWidth = (toFloat boardSize) * tileSize + (1 + toFloat boardSize) * tileMargin

displayGrid: GameState -> Element -- display a grid
displayGrid gameState = let
                    gridBox = filled (rgb 187 173 160) -- the grid background
                                <| square boardWidth
                    tiles = map (\position -> displayTileAtCoordinates position gameState)
                        <| tilesWithCoordinates gameState.board
    in collage (round boardWidth) (round boardWidth) ([gridBox] ++ tiles)

displayOverlay: Style -> Color -> String ->  Element -- display an overlay
                                                      -- with a message
displayOverlay s c t = collage (round boardWidth) (round boardWidth)
    [
      filled c <| square boardWidth -- background
    , toForm <| centered <| style s <| fromString t -- message
    ]

wonOverlayStyle: Style
wonOverlayStyle = wonTextStyle <| Just (Piece Black King)

wonOverlayColor: Color
wonOverlayColor = rgba 237 194 46 0.5

displayWonOverlay: String -> Element -- display a game won overlay
displayWonOverlay message = displayOverlay
                            wonOverlayStyle
                            wonOverlayColor
                            message

applyOverlay: Element -> Element -> Element
applyOverlay overlay board = collage (round boardWidth) (round boardWidth)
        [
          toForm <| board
        , toForm <| overlay
        ]

display: GameState -> Element -- display a gamestate
display gameState = displayGrid gameState
                    |> case gameState.gameProgress of
                        WhiteWon -> applyOverlay (displayWonOverlay "White wins!")
                        BlackWon-> applyOverlay (displayWonOverlay "Black wins")
                        otherwise -> identity
