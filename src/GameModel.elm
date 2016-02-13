module GameModel where

import Utils exposing (Position)
import Piece exposing (PieceColor)
import Board exposing (Board, startBoard)
import Random exposing (Seed)

type Progress = InProgress | WhiteWon | BlackWon | Draw
type alias PlayerColor = PieceColor
type GameType = OneVSOne | OneVSComputer

type alias GameState = {
    board: Board,
    gameProgress: Progress,
    turn: PlayerColor,
    selected: Maybe Position,
    cursorAt: Position,
    gameType: GameType,
    mayhem: Bool,
    seed: Maybe Seed
}

defaultGame: GameState -- the default starting game state
defaultGame = {
    board = startBoard,
    gameProgress = InProgress,
    turn = Piece.White,
    selected = Nothing,
    cursorAt = (6,4),
    gameType = OneVSOne,
    mayhem = False,
    seed = Nothing
    }