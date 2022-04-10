module World where

import qualified System.Random as R

data World = NewWorld
    { resolution :: (Int, Int)
    , scale :: Int
    , snake :: [(Int, Int)]
    , isOver :: Bool
    , gen :: R.StdGen
    , food :: (Int, Int)
    , stepCount :: Int
    } deriving (Show)

inBounds :: World -> (Int, Int) -> Bool
inBounds world (x, y) =
    let s = scale world `div` 2
    in  -s <= x && x <= s && -s <= y && y <= s

isSnake :: World -> (Int, Int) -> Bool
isSnake world (x, y) = (x, y) `elem` snake world

isFood :: World -> (Int, Int) -> Bool
isFood world (x, y) = (x, y) == food world

getHead :: World -> (Int, Int)
getHead world = head (snake world)

