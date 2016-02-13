module GameManager where

import GameModel exposing (..)
import InputModel exposing (..)
import Utils exposing (..)
import Piece exposing (PieceColor)
import Tile exposing (Tile)
import Board exposing (Board, readTile, getPiecesForColor, promotePossiblePawns)
import Logic exposing (inCheck, canMakeMove, canMoveTile, loopUntilCanMovePiece, colorInCheck)

import Debug
import Random exposing (Seed, initialSeed)

checkValidGame: Input -> GameState -> GameState
checkValidGame input gameState =  if gameFinished gameState
                                      then { gameState | gameProgress <- colorLost gameState.turn }
                                      else (checkPreConditions input gameState) |> stepGame input |> checkPostCondition

checkPreConditions: Input -> GameState -> GameState
checkPreConditions input gameState =
                              let
                                newGameState = { gameState | seed <- Just (initialSeed input.currentTimestamp)
                                                }
                              in
                                case input.action of
                                MayhemActivated True -> switchMayhem newGameState
                                NewGameButtonPressed True ->  let
                                                                initialGame = startingGame input
                                                              in
                                                                { initialGame | gameType <- gameState.gameType }
                                GameTypeChanged True -> switchGameType newGameState
                                otherwise -> newGameState

stepGame: Input -> GameState -> GameState
stepGame input gameState =  if
                              | gameState.mayhem
                                -> simulateComputerPlayer input gameState
                              | gameState.gameType == OneVSComputer && gameState.turn == Piece.Black
                                -> simulateComputerPlayer input gameState
                              | otherwise
                                -> case input.action of
                                    Move direction -> moveCursor gameState direction
                                    EnterPressed True -> playerMakingMove input gameState
                                    otherwise -> gameState

--Color is already switched
checkPostCondition: GameState -> GameState
checkPostCondition gameState = gameState

switchMayhem: GameState -> GameState
switchMayhem g = let x = not g.mayhem in { g | mayhem <- x }

gameFinished: GameState -> Bool
gameFinished gameState = case gameState.seed of
                          Nothing -> False
                          Just seed ->
                                      let
                                        maybePiece = loopUntilCanMovePiece gameState.board gameState.turn seed
                                      in
                                        case maybePiece of
                                          Nothing -> True
                                          otherwise -> False

moveCursorToOrigin: GameState -> GameState
moveCursorToOrigin gameState = case gameState.turn of
                          Piece.Black -> if gameState.gameType /= OneVSComputer
                                      then { gameState | cursorAt <- (1,4) }
                                      else gameState
                          Piece.White -> { gameState | cursorAt <- (6,4) }

playerMakingMove: Input -> GameState -> GameState
playerMakingMove input gameState =  let
                                      selected = gameState.selected -- Position selected
                                      tileAt = readTile gameState.cursorAt gameState.board -- tile where cursor is
                                    in case (selected, tileAt) of
                                    (Nothing, Nothing) -> gameState
                                    (Nothing, Just piece) -> { gameState | selected <- Just (gameState.cursorAt) }
                                    (Just position, Nothing) -> attemptMove gameState (readTile position gameState.board) position gameState.cursorAt (\gamestate -> { gamestate | selected <- Nothing })
                                    (Just position, Just piece) -> attemptMove gameState (readTile position gameState.board) position gameState.cursorAt (\gamestate -> { gamestate | selected <- Just (gameState.cursorAt) })

startingGame: Input -> GameState
startingGame input = defaultGame

switchGameType: GameState -> GameState
switchGameType gameState = { gameState | gameType <- case gameState.gameType of
                                                              OneVSOne -> OneVSComputer
                                                              OneVSComputer -> OneVSOne
                            }

simulateComputerPlayer: Input -> GameState -> GameState
simulateComputerPlayer input gameState = makeRandomMove gameState

makeRandomMove: GameState -> GameState
makeRandomMove gameState = case gameState.seed of
                              Nothing -> gameState
                              Just seed ->
                                let
                                  maybeMovement = loopUntilCanMovePiece gameState.board gameState.turn seed
                                in case maybeMovement of
                                  Nothing -> { gameState | gameProgress <- colorLost gameState.turn }
                                  Just (from, to, newSeed) -> let newGameState = makeMove gameState (readTile from gameState.board) from to in { newGameState | seed <- Just (newSeed) }

colorLost: PlayerColor -> Progress
colorLost color = case color of
                  Piece.Black -> WhiteWon
                  Piece.White -> BlackWon


attemptMove: GameState -> Tile -> Position -> Position -> (GameState -> GameState) -> GameState
attemptMove gameState tile from to f =
                            case gameState.seed of
                              Nothing -> gameState
                              Just seed ->
                                let
                                  possibleMovement = loopUntilCanMovePiece gameState.board gameState.turn seed
                                in
                                  case possibleMovement of
                                    Nothing -> { gameState | gameProgress <- Debug.log "lost" (colorLost gameState.turn) }
                                    Just (possible) -> if canMoveTile gameState.board gameState.turn tile from to
                                                        then f (makeMove gameState tile from to)
                                                        else f gameState

makeMove: GameState -> Tile -> Position -> Position -> GameState
makeMove g tile from to = let
                            player = g.turn
                            board = promotePossiblePawns (Board.makeMove g.board from to) tile to
                          in
                          moveCursorToOrigin {
                            g | board <- board,
                            selected <- Nothing,
                            turn <- case g.turn of
                                  Piece.White -> Piece.Black
                                  Piece.Black -> Piece.White
                          }

moveCursor: GameState -> Direction -> GameState
moveCursor gameState direction =
            let
              newPosition = getNewPosition gameState.cursorAt (getIncrement direction)
            in
              if direction /= None && isInsideBoard newPosition
                then { gameState | cursorAt <- newPosition }
                else gameState
