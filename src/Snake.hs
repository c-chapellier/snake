module Snake where

import qualified Graphics.Gloss.Interface.Pure.Game as G
import qualified System.Random as R

import World
import SnakeBot ( chooseDirection )

main :: IO ()
main = do
    seed <- R.randomIO
    let world = initialWorld seed

    G.play
        (displayMode world)
        backgroundColor
        10
        world
        drawWorld
        handleEvent
        handleStep

--

displayMode :: World -> G.Display
displayMode world = G.InWindow "Snake" (resolution world) (0, 0)

backgroundColor :: G.Color
backgroundColor = G.white

initialWorld :: Int -> World
initialWorld seed = moveFood NewWorld
    { resolution = (512, 512)
    , scale = 11
    , snake = [(0, 2), (0, 1), (0, 0), (0, -1), (0, -2)]
    , isOver = False
    , gen = R.mkStdGen seed
    , food = (0, 0)
    , stepCount = 0
    }

drawWorld :: World -> G.Picture
drawWorld world = G.pictures
    [ drawBounds world
    , drawFood world
    , drawSnake world
    , drawGameOver world
    ]

handleEvent :: G.Event -> World -> World
handleEvent event world = case event of
    G.EventResize newResolution -> handleResize newResolution world
    _ -> world

handleStep :: Float -> World -> World
handleStep _time world =
    if isOver world
    then world
    else
        let oldSnake = snake world
            newSnake@((x, y) : _) = init oldSnake
            (x', y') = chooseDirection world
        in  if inBounds world (x', y') && not (isSnake world (x', y'))
            then if isFood world (x', y')
                then
                    let world' = moveFood world
                    in  world' { snake = (x', y') : oldSnake, stepCount = stepCount world + 1 }
                else world { snake = (x', y') : newSnake, stepCount = stepCount world + 1 }
            else world { isOver = True }

--

drawBounds :: World -> G.Picture
drawBounds world =
    let x = size world
    in  G.rectangleWire x x

drawFood :: World -> G.Picture
drawFood world = G.color G.green (drawBox (food world) world)

drawSnake :: World -> G.Picture
drawSnake world = case snake world of
    (p : ps) -> G.pictures
        ( G.color G.orange (drawBox p world)
        : map (`drawBox` world) ps
        )
    _ -> G.blank

drawBox :: (Int, Int) -> World -> G.Picture
drawBox (x, y) world =
    let s = size world / fromIntegral (scale world)
        x' = s * fromIntegral x
        y' = s * fromIntegral y
    in  G.translate x' y' (G.rectangleSolid s s)

drawGameOver :: World -> G.Picture
drawGameOver world = if isOver world
    then G.pictures
        [ G.color G.red (G.scale 0.2 0.2 (G.text "game over"))
        , G.color G.blue (G.translate 0 (-50) (G.scale 0.2 0.2 (G.text ("score: " ++ show (length (snake world))))))
        ]
    else G.blank

--

handleResize :: (Int, Int) -> World -> World
handleResize newResolution world = world { resolution = newResolution }

--

size :: (Num a) => World -> a
size world =
    let (width, height) = resolution world
    in  fromIntegral (min width height)

moveFood :: World -> World
moveFood world =
    let g0 = gen world
        a = scale world `div` 2
        (x, g1) = R.randomR (-a, a) g0
        (y, g2) = R.randomR (-a, a) g1
    in  if isSnake world (x, y)
        then moveFood world { gen = g2 }
        else world { gen = g2 , food = (x, y) }

