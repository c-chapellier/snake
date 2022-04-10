module SnakeBot where

import World
import Data.Sort
import Debug.Trace

getAllPossibleMoves :: World -> (Int, Int) -> [(Int, Int)]
getAllPossibleMoves world (x, y) = Prelude.filter (\x -> not (isSnake world x) && inBounds world x) [(x, y + 1), (x + 1, y), (x, y - 1), (x - 1, y)]

distToHead :: World -> (Int, Int) -> Int
distToHead world (x, y) = let (xh, yh) = food world
  in (x-xh)^2 + (y-yh)^2

sortByDist :: World -> [(Int, Int)]
sortByDist world = let sorted = sortBy (\a b -> compare (distToHead world a) (distToHead world b)) (getAllPossibleMoves world (getHead world))
  in trace ("sorted " ++ show sorted) sorted

chooseDirection :: World -> (Int, Int)
chooseDirection world = head (sortByDist world)
