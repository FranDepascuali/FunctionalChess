module Utils where

import InputModel exposing (..)

import Array exposing (Array, get)
import Maybe exposing (Maybe, map, withDefault)
import Random exposing (Seed, generate, int)
import List exposing (head, tail, concat, map, take, drop)

type alias Position = (Int, Int)

allPositions: List (Int, Int)
allPositions = concat (List.map (\x -> List.map (\y -> (x,y)) [0..(boardSize-1)]) [0..(boardSize-1)])

boardSize: Int
boardSize = 8

infixl 9 !
(!): Array a -> Int -> Maybe a -- the nth element of a list
arr ! n = get n arr

getElement: Maybe(List a) -> Int -> Maybe a
getElement maybeList n = case maybeList of
                          Nothing -> Nothing
                          Just list -> if n == 0 then head list else getElement (tail list) (n - 1)

plus: Position -> Position -> Position
plus lhs rhs = (fst lhs + fst rhs, snd lhs + snd rhs)

minus: Position -> Position -> Position
minus lhs rhs = (fst lhs - fst rhs, snd lhs - snd rhs)

infixl 9 |||
(|||): Bool -> Bool -> Bool
a ||| b = (a || b) && not (a && b)

interval: Int -> Int -> List (Int)
interval x1 x2 = [min x1 x2 .. max x1 x2]

getRow: Position -> Int
getRow x = fst x

getColumn: Position -> Int
getColumn x = snd x

sameRow: Position -> Position -> Bool
sameRow x y = getRow x == getRow y

sameColumn: Position -> Position -> Bool
sameColumn x y = getColumn x == getColumn y

upperRightOf: Position -> Position -> Bool
upperRightOf x y = x `atRightOf` y && x `atUpOf` y

upperLeftOf: Position -> Position -> Bool
upperLeftOf x y = x `atLeftOf` y && x `atUpOf` y

downRightOf: Position -> Position -> Bool
downRightOf x y = x `atRightOf` y && x `atDownOf` y

downLeftOf: Position -> Position -> Bool
downLeftOf x y = x `atLeftOf` y && x `atDownOf` y

atRightOf: Position -> Position -> Bool
atRightOf x y = getColumn x > getColumn y

atLeftOf: Position -> Position -> Bool
atLeftOf x y = getColumn x < getColumn y

atUpOf: Position -> Position -> Bool
atUpOf x y = getRow x < getRow y

rowsBetween: Position -> Position -> Int
rowsBetween x y = getRow x - getRow y

atDownOf: Position -> Position -> Bool
atDownOf x y = getRow x > getRow y

sign: Int -> Int
sign x = if x > 0 then 1 else (-1)

isEven: Int -> Bool
isEven x = (x % 2 ) == 0

isOdd: Int -> Bool
isOdd x = not (isEven x)

mapWithDefault: (a -> b) -> Maybe a -> b -> b
mapWithDefault f m h = withDefault h (Maybe.map f m)

isInsideBoard: Position -> Bool
isInsideBoard (x,y) = x >= 0 && x < boardSize && y >= 0 && y < boardSize

-- This is implemented because elm is not lazy
firstThatSatisfies: (a -> Bool) -> Maybe (List a) -> Maybe a
firstThatSatisfies f maybeList = case maybeList of
                                  Nothing -> Nothing
                                  Just list -> let
                                                element = head list
                                              in if mapWithDefault f element False
                                                then element
                                                else firstThatSatisfies f (tail list)

generateRandomPosition: Position -> Position -> Seed -> (Position, Seed)
generateRandomPosition from to seed = let
                                                  (x, newSeed) = generateRandomInt (fst from) (fst to) seed
                                                  (y, newSeed2) = generateRandomInt (snd from) (snd to) newSeed
                                                in
                                                  ((x,y), newSeed)

generateRandomInt: Int -> Int -> Seed -> (Int, Seed)
generateRandomInt from to seed = generate (int from to) seed

shuffle: List a -> Seed -> (List a, Seed)
shuffle list seed = if List.isEmpty list
                      then ([], seed)
                      else
                      let
                        (randomIndex, newSeed) = generateRandomInt 0 (List.length list - 1) seed
                        maybeElement = getElement (Just list) randomIndex
                      in
                        case maybeElement of
                          Nothing -> ([], newSeed)
                          Just (element) ->
                            let
                              shuffled = shuffle (without randomIndex list) newSeed
                            in ([element] ++ fst shuffled, snd shuffled)

without: Int -> List a -> List a
without i list =
  let before = take i list
      after = drop (i+1) list
  in
    before ++ after

getNewPosition: Position -> Position -> Position
getNewPosition (from, to) (incX, incY) = (from + incX, to + incY)

getIncrement: Direction -> Position
getIncrement direction = case direction of
                          Up -> ((-1), 0)
                          Down -> (1, 0)
                          Left -> (0, -1)
                          Right -> (0, 1)
                          None -> (0, 0)