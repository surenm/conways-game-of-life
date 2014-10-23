import Dict
import Graphics.Collage
import Window
import Debug

-- Type alias for Grid and position to represent our game
type Position = (Float, Float)
type Grid = Dict.Dict (Float, Float) ()

newGrid: [Position] -> Grid
newGrid ps = Dict.fromList (map (\p -> (p, ())) ps)

liveCells: Grid -> [Position]
liveCells g = Dict.keys g

alive: Grid -> Position -> Bool
alive g p = Dict.member p g

validCell: Position -> Bool
validCell (x, y) = x >= 0 && y >= 0

neighbours: Position -> [Position]
neighbours p = filter validCell (map (\d -> p `add` d) deltas)

aliveNeighbours: Grid -> Position -> [Position]
aliveNeighbours g p = filter (\p -> alive g p) (neighbours p)

aliveInNextGeneration:  Grid -> Position -> Bool
aliveInNextGeneration g p =
  let liveNeighbors = length (aliveNeighbours g p)
  in (alive g p && liveNeighbors == 2) || liveNeighbors == 3

-- Constants. Sort of.
gridSize = 500
cellSize = 50
gridDimension = gridSize/cellSize
funColor = rgba 81 116 22 1.0
deltas    = [(1, 0), (1, -1), (0, -1), (-1, -1), (-1, 0), (-1, 1), (0, 1), (1, 1)]
glider    = [(1, 0), (2, 1), (0, 2), (1, 2), (2, 2)]
positions = concat (map (\y -> map (\x -> (x, y)) [0..gridDimension]) [0..gridDimension])
initialGrid = newGrid glider

-- Tuple addition helper
add: Position -> Position -> Position
add (a1, b1) (a2, b2) = (a1 + a2, b1 + b2)

-- Drawing helpers
verticalLine : Float -> Float -> Path
verticalLine height ordinate = path [ (ordinate, -height/2), (ordinate, height/2) ]

horizontalLine : Float -> Float -> Path
horizontalLine width abscissca = path [ (-width/2, abscissca), (width/2, abscissca) ]

renderGrid : Float -> [Path]
renderGrid size =
  let lines = map (\n -> n * cellSize) [(-size/(2 * cellSize))..(size/(2 * cellSize))]
  in (map (verticalLine size) lines) ++ (map (horizontalLine size) lines)

renderLiveCell : (Float, Float) -> Form
renderLiveCell (x, y) =
  move (-gridSize/2 + (x * cellSize) + cellSize/2, gridSize/2 - (y * cellSize) - cellSize/2) (filled funColor (circle (cellSize/2.5)))

renderLife: Grid -> [Form]
renderLife g = map renderLiveCell (liveCells g)

-- Evolve next generation from the current generation
nextGeneration: Float -> Grid -> Grid
nextGeneration pulse g = newGrid (filter (\p -> aliveInNextGeneration g p) positions)

-- render
render : Grid -> Int -> Int -> Element
render g width height = container width height middle (
  collage gridSize gridSize ( map (traced (solid black)) (renderGrid gridSize) ++ (renderLife g)))

-- Render the animation now
main = lift3 render (foldp (nextGeneration) initialGrid (fps 2)) Window.width Window.height