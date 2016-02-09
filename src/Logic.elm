module Logic where

import Piece exposing (..)
import Tile exposing (..)
import Board exposing (..)
import Utils exposing (..)

import Maybe exposing (Maybe, withDefault)
import Array exposing (Array, toList)
import Debug
import Random exposing (Seed)
import List exposing (filter, any, all, head, drop, take, foldr, tail)

canMoveTile: Board -> PieceColor -> Tile -> Position -> Position -> Bool
canMoveTile board color tile from to = mapWithDefault (\piece -> canMakeMove board color piece from to) tile False

canMakeMove: Board -> PieceColor -> Piece -> Position -> Position -> Bool
canMakeMove board color piece from to =
    let
      tileTo = readTile to board
    in
      pieceColorMatches color piece &&
      not (colorMatches color tileTo) &&
      pieceCanReachPosition board piece from to &&
      let
        newBoard = makeMove board from to
      in
        notInCheck newBoard color

pieceCanReachPosition: Board -> Piece -> Position -> Position -> Bool
pieceCanReachPosition board piece from to = notOtherPieceInPath board piece from to && (moveIsValid board piece from to || canAttack board piece from to)

notOtherPieceInPath: Board -> Piece -> Position -> Position -> Bool
notOtherPieceInPath board piece from to = let
                                      positions = piecePossibleMoves piece from to
                                    in
                                    case piece of
                                      Piece _ Knight -> True
                                      Piece _ King -> True
                                      otherwise -> all (\position -> readTile position board == Nothing) positions

notInCheck: Board -> PieceColor -> Bool
notInCheck board color = not (inCheck board color)

inCheck: Board -> PieceColor -> Bool
inCheck board color = let
                        pieces = getPiecesFromBoard board
                        otherColorPieces = getPiecesForColor board (switchColor color)
                        king = head (filter (\(t, x, y) -> t == Piece color King) pieces)
                      in
                        case king of
                          Nothing -> False -- Should not enter here, but king is Maybe.
                          Just kingPosition -> any (\(piece, x, y) -> pieceCanReachPosition board piece (x,y) (second kingPosition, thrd kingPosition)) otherColorPieces

canAttack: Board -> Piece -> Position -> Position -> Bool
canAttack board piece from to = case piece of
                            Piece White Pawn -> whitePawnCanAttack from to board
                            Piece Black Pawn -> blackPawnCanAttack from to board
                            otherwise -> False

moveIsValid: Board -> Piece -> Position -> Position -> Bool
moveIsValid board piece from to = let
                              difRow = fst to - fst from
                              difCol = snd to - snd from
                            in case piece of
                              Piece _ Rook ->  canMoveRook difCol difRow
                              Piece _ Knight -> canMoveKnight (abs difCol) (abs difRow)
                              Piece _ Bishop -> canMoveBishop (abs difCol) (abs difRow)
                              Piece _ Queen -> canMoveQueen (abs difCol) (abs difRow)
                              Piece White Pawn -> canMoveWhitePawn board from to
                              Piece Black Pawn -> canMoveBlackPawn board from to
                              Piece _ King -> canMoveKing (abs difRow) (abs difCol)

loopUntilCanMovePiece: Board -> PieceColor -> Seed -> List(Piece, Int, Int) -> Maybe(Position, Position, Seed)
loopUntilCanMovePiece board color seed pieces = let
                                                  (shuffledPieces, newSeed) = shuffle pieces seed
                                                in
                                                  evaluatePossibleMovement board color shuffledPieces newSeed

evaluatePossibleMovement: Board -> PieceColor -> List(Piece, Int, Int) -> Seed -> Maybe(Position, Position, Seed)
evaluatePossibleMovement board color pieces seed = case head pieces of
                                        Nothing -> Nothing
                                        Just (piece,x,y) -> let
                                                                (randomPositions, newSeed) = getRandomPositionsForPiece board piece (x, y) seed
                                                                move = firstThatSatisfies (\randomPosition -> canMakeMove board color piece (x,y) randomPosition) (Just randomPositions)
                                                              in case move of
                                                                    Nothing -> evaluatePossibleMovement board color (getTail pieces) newSeed
                                                                    Just position -> Just ((x,y), position, newSeed)

getTail: List a -> List a
getTail list = withDefault [] (tail (list))

getRandomPositionsForPiece: Board -> Piece -> Position -> Seed -> (List Position, Seed)
getRandomPositionsForPiece board piece from seed = shuffle (generatePossiblePositions board piece from) seed

generatePossiblePositions: Board -> Piece -> Position -> List Position
generatePossiblePositions board piece from = allPositions |> List.filter (moveIsValid board piece from)