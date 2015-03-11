module Board where

import Piece exposing (..)
import Tile exposing (..)
import Utils exposing (..)

import Array exposing (Array, set, repeat)
import Maybe exposing (Maybe, andThen)
import List exposing (foldr, filter, filterMap)

type alias Board = Array (Array Tile)

emptyBoard: Board
emptyBoard = repeat boardSize <| repeat boardSize <| Nothing

startBoard: Board
startBoard = setTiles (initialTiles) emptyBoard

readTile: Position -> Board -> Tile -- (!) returns Maybe, so arr ! j is a Maybe Tile and we need Tile
readTile (i,j) g = g ! i `andThen` \arr -> arr ! j `andThen` identity

setTile: Position -> Tile -> Board -> Board
setTile (i,j) t g = let r = g ! i in
                      case r of
                        Nothing -> g
                        Just arr -> set i (set j t arr) g

setTiles: List (Position, Tile) -> Board -> Board
setTiles arr b = foldr (\(position, tile) board -> setTile position tile board) b arr

tilesWithCoordinates: Board -> List (Tile, Int, Int)
tilesWithCoordinates b = foldr (\i tiles -> tiles ++ getTilesForRow i b) [] [0..(boardSize-1)]

getTilesForRow: Int -> Board -> List (Tile, Int, Int)
getTilesForRow i b = foldr (\j arr -> arr ++ [(readTile (i,j) b, i , j)]) [] [0..(boardSize-1)]

makeMove: Board -> Position -> Position -> Board
makeMove board from to =  let
                            tile = readTile from board
                          in
                            setTiles [(from, Nothing), (to, tile)] board

whitePawnCanAttack: Position -> Position -> Board -> Bool
whitePawnCanAttack from to board = whitePawnAttackPositions from to && (readTile to board) /= Nothing

blackPawnCanAttack: Position -> Position -> Board -> Bool
blackPawnCanAttack from to board = blackPawnAttackPositions from to && (readTile to board) /= Nothing

getPiecesFromBoard: Board -> List (Piece, Int, Int)
getPiecesFromBoard board = filterMap (\(t, x, y) -> case t of
                                                        Nothing -> Nothing
                                                        Just piece -> Just (piece, x, y)) (tilesWithCoordinates board)

getPiecesForColor: Board -> PieceColor -> List (Piece, Int, Int)
getPiecesForColor board color = filter (\(t, x, y) -> pieceColorMatches color t) (getPiecesFromBoard board)

promotePossiblePawns: Board -> Tile -> Position -> Board
promotePossiblePawns board tile position = case (position, tile) of
                                        ((0, _), Just (Piece White Pawn)) -> setTile position (Just (Piece White Queen)) board
                                        ((7, _), Just (Piece Black Pawn)) -> setTile position (Just (Piece Black Queen)) board
                                        otherwise -> board

canMoveWhitePawn: Board -> Position -> Position -> Bool
canMoveWhitePawn board from to = readTile to board == Nothing &&
                                  ((to `minus` from) == (-1, 0) ||
                                    case (isFirstMove (Piece White Pawn) from) of
                                        True -> (to `minus` from) == (-2, 0)
                                        False -> False
                                  )

canMoveBlackPawn: Board -> Position -> Position -> Bool
canMoveBlackPawn board from to = readTile to board == Nothing &&
                                  ((to `minus` from) == (1, 0) ||
                                    case (isFirstMove (Piece Black Pawn) from) of
                                        True -> (to `minus` from) == (2, 0)
                                        False -> False
                                  )