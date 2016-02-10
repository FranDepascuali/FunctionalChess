module Piece where

import Utils exposing (..)
import List exposing (filter, any)

type PieceColor = White | Black
type PieceType = Pawn | Knight | Bishop | Rook | Queen | King
type Piece = Piece PieceColor PieceType

switchColor: PieceColor -> PieceColor
switchColor color = case color of
                      Black -> White
                      White -> Black

stringToPiece: String -> String -> Piece
stringToPiece c t = Piece (stringToColor c) (stringToType t)

stringToColor: String -> PieceColor
stringToColor c = case c of
                    "White" -> White
                    "Black" -> Black

stringToType: String -> PieceType
stringToType t = case t of
                    "Rook" -> Rook
                    "Knight" -> Knight
                    "King" -> King
                    "Bishop" -> Bishop
                    "Queen" -> Queen
                    "Pawn" -> Pawn

pieceColorMatches: PieceColor -> Piece -> Bool
pieceColorMatches color (Piece c t) = c == color

pieceToString: Piece -> (String, String)
pieceToString (Piece c t) = (toString c, toString t)

initialPieces: List (Position, Piece)
initialPieces = initialPiecesFromColor White ++ initialPiecesFromColor Black

initialPiecesFromColor: PieceColor -> List (Position, Piece)
initialPiecesFromColor c = initialSpecialPiecesFromColor c ++ initialPawnsFromColor c

initialSpecialPiecesFromColor: PieceColor -> List (Position, Piece)
initialSpecialPiecesFromColor c = List.map2 (\j t -> ((getInitialRowFromColor c t, j), Piece c t)) [0..(boardSize - 1)] [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook]

initialPawnsFromColor: PieceColor -> List (Position, Piece)
initialPawnsFromColor c = List.map (\j -> ((getInitialRowFromColor c Pawn, j), Piece c Pawn)) [0..(boardSize - 1)]

getInitialRowFromColor: PieceColor -> PieceType -> Int
getInitialRowFromColor c t = case (c,t) of
                                (Black, Pawn) -> 1
                                (White, Pawn) -> 6
                                (Black, _) -> 0
                                (White, _) -> boardSize - 1

-- We only care for pawns first moves, as this functions is used for checking if pawn can move one or two spaces.
isFirstMove: Piece -> Position -> Bool
isFirstMove piece (x,y) = case piece of
                        Piece White Pawn -> x == 6
                        Piece Black Pawn -> x == 1
                        otherwise -> False

first: (Piece, Int, Int) -> Piece -- If named fst, it conflicts with fst
first (piece, x, y) = piece

second: (Piece, Int, Int) -> Int -- If named snd, it conflicts with snd
second (piece, x, y) = x

thrd: (Piece, Int, Int) -> Int
thrd (piece, x, y) = y

canMoveRook: Int -> Int -> Bool
canMoveRook difCol difRow = difCol == 0 || difRow == 0 && not (difCol == 0 && difRow == 0)

canMoveQueen: Int -> Int -> Bool
canMoveQueen difx dify = canMoveRook difx dify || canMoveBishop difx dify

canMoveKnight: Int -> Int -> Bool
canMoveKnight difCol difRow = case (difCol,difRow) of
                              (1,2) -> True
                              (2,1) -> True
                              otherwise -> False

canMoveBishop: Int -> Int -> Bool
canMoveBishop difx dify = difx /= 0 && difx == dify

canMoveKing: Int -> Int -> Bool
canMoveKing difCol difRow = case (difCol, difRow) of
                            (1,1) -> True
                            (1,0) -> True
                            (0,1) -> True
                            otherwise -> False

piecePossibleMoves: Piece -> Position -> Position -> List (Position)
piecePossibleMoves piece from to =
                            case piece of
                            Piece _ Rook -> rookPossibleMoves from to
                            Piece _ Bishop -> bishopPossibleMoves from to
                            Piece _ Queen -> queenPossibleMoves from to
                            Piece White Pawn -> whitePawnPossibleMoves from to
                            Piece Black Pawn -> blackPawnPossibleMoves from to
                            otherwise -> []

whitePawnAttackPositions: Position -> Position -> Bool
whitePawnAttackPositions from to = to == (from `minus` (1,1)) || to == (from `minus` (1,-1))

blackPawnAttackPositions: Position -> Position -> Bool
blackPawnAttackPositions from to = to == (from `plus` (1,1)) || to == (from `plus` (1,-1))

queenPossibleMoves: Position -> Position -> List (Position)
queenPossibleMoves from to = rookPossibleMoves from to ++ bishopPossibleMoves from to

whitePawnPossibleMoves: Position -> Position -> List (Position)
whitePawnPossibleMoves from to = case abs (rowsBetween from to) of
                                  1 -> []
                                  2 -> [(fst from - 1, snd from)]
                                  otherwise -> []

blackPawnPossibleMoves: Position -> Position -> List (Position)
blackPawnPossibleMoves from to = case abs (rowsBetween from to) of
                                  1 -> []
                                  2 -> [(fst from + 1, snd from)]
                                  otherwise -> []

rookPossibleMoves: Position -> Position -> List (Position)
rookPossibleMoves from to = let
                              difRow = fst to - fst from
                              difCol = snd to - snd from
                            in case (difRow, difCol) of
                              (0, _) -> filter (\position -> sameRow from position && any (\x -> x == getColumn position) (interval (getColumn from) (getColumn to))) (rookMoves from to)
                              (_, 0) -> filter (\position -> sameColumn from position && any (\x -> x == getRow position) (interval (getRow from) (getRow to))) (rookMoves from to)
                              otherwise -> []

bishopPossibleMoves: Position -> Position -> List (Position)
bishopPossibleMoves from to = let
                                difRow = fst to - fst from
                                difCol = snd to - snd from
                              in case (sign difRow, sign difCol) of
                              (1, 1) -> filter (\position -> position `downRightOf` from && position `upperLeftOf` to) (bishopMoves from to)
                              (-1, -1) -> filter (\position -> position `upperLeftOf` from && position `downRightOf` to) (bishopMoves from to)
                              (1, -1) -> filter (\position -> position `downLeftOf` from && position `upperRightOf` to) (bishopMoves from to)
                              (-1, 1) -> filter (\position -> position `upperRightOf` from && position `downLeftOf` to) (bishopMoves from to)
                              otherwise -> []

bishopMoves: Position -> Position -> List (Position)
bishopMoves from to = filter (\x -> x /= to && x /= from && canMoveBishop (abs (fst x - fst from)) (abs (snd x - snd from))) allPositions

-- All positions between the rook and its objective, without from and to
rookMoves: Position -> Position -> List (Position)
rookMoves from to = filter (\x -> x /= to && x /= from && canMoveRook (fst x - fst from) (snd x - snd from)) allPositions