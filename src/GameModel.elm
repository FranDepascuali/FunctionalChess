module GameModel where

import Utils exposing (Position)
import Piece exposing (PieceColor)
import Board exposing (Board, startBoard)
import Random exposing (Seed)

type Progress = Stopped | InProgress | WhiteWon | BlackWon
type alias PlayerColor = PieceColor
type GameType = OneVSOne | OneVSComputer
type Player = Human | Computer

type alias GameState = {
    board: Board,
    gameProgress: Progress,
    turn: PieceColor,
    selected: Maybe Position,
    cursorAt: Position,
    gameType: GameType,
    player: Player,
    inCheck: Maybe PlayerColor,
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
    player = Human,
    inCheck = Nothing,
    mayhem = False,
    seed = Nothing
    }