module Tile where

import Piece exposing (..)
import Utils exposing (..)

import Maybe exposing (Maybe)
import List exposing (map)

type alias Tile = Maybe Piece

stringToTile: (String, String) -> Tile
stringToTile (c,t) = case (c,t) of
                        ("","") -> Nothing
                        otherwise -> Just (stringToPiece c t)

tileToString: Tile -> (String, String)
tileToString t = case t of
                Nothing -> ("", "")
                Just p -> pieceToString p

initialTiles: List (Position, Tile)
initialTiles = map (\(position, piece) -> (position, Just piece)) initialPieces

colorMatches: PieceColor -> Tile -> Bool
colorMatches color tile = mapWithDefault (pieceColorMatches color) tile False