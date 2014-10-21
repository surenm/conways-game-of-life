import Dict
import Graphics.Collage
import Window
import Debug

cellSize : number
cellSize = 50

gridSize : number
gridSize = 500

funColor: Color
funColor = rgba 81 116 22 1.0

add: (number, number) -> (number, number) -> (number, number)
add (a1, b1) (a2, b2) = (a1 + a2, b1 + b2)

initialGrid: Dict.Dict (number, number) ()
initialGrid = Dict.fromList [((1, 0), ()), ((2, 1), ()), ((0, 2), ()), ((1, 2), ()), ((2, 2), ())]

verticalLine : Float -> Float -> Path
verticalLine height ordinate = path [ (ordinate, -height/2), (ordinate, height/2) ]

horizontalLine : Float -> Float -> Path
horizontalLine width abscissca = path [ (-width/2, abscissca), (width/2, abscissca) ]

renderGrid : Float -> [Path]
renderGrid size =
  let verticalLines = map (verticalLine size) (map (\n -> n * cellSize) [(-size/(2 * cellSize))..(size/(2 * cellSize))])
      horizontalLines = map (horizontalLine size) (map (\n -> n * cellSize) [(-size/(2 * cellSize))..(size/(2 * cellSize))]) in

    horizontalLines ++ verticalLines

renderLiveCell : (Float, Float) -> Form
renderLiveCell (x, y) = move ((-gridSize/2 + x + cellSize/2, gridSize/2 - y - cellSize/2)) (filled funColor (circle (cellSize/2.5)))

renderLife: Dict.Dict (number, number) () -> [Form]
renderLife  grid = map renderLiveCell (map (\(x,y) -> (x * cellSize, y * cellSize)) (Dict.keys grid))

hasLiveCell : Dict.Dict (number, number) () -> (number, number) -> Bool
hasLiveCell grid position = Dict.member position grid

validCell: (number, number) -> Bool
validCell (x, y) = x >= 0 && y >= 0 && x < gridSize//cellSize && y < gridSize//cellSize

neighbouringCells: (number, number) -> [(number, number)]
neighbouringCells position =
  let deltas = [(1, 0), (1, -1), (0, -1), (-1, -1), (-1, 0), (-1, 1), (0, 1), (1, 1)] in
    if validCell(position)
      then filter validCell (map (\delta -> add position delta) deltas)
    else
      []

neighbouringLiveCells: Dict.Dict (number, number) () -> (number, number) -> [(number, number)]
neighbouringLiveCells grid position =
  filter (\position -> hasLiveCell grid position) (neighbouringCells position)

livesInNextGeneration:  Dict.Dict (number, number) () -> (number, number) -> Bool
livesInNextGeneration grid position =
  (hasLiveCell grid position && length (neighbouringLiveCells grid position) == 2) ||
  length (neighbouringLiveCells grid position) == 3

evolve: Dict.Dict (number, number) () -> Dict.Dict (number, number) ()
evolve grid =
  let xs = [0..(gridSize/cellSize)]
      ys = [0..(gridSize/cellSize)] in
    Dict.fromList (filter (\pos -> livesInNextGeneration grid (fst pos)) (concat (map (\y -> map (\x -> ((x, y), ())) xs) ys)))

nextGeneration pulse life = evolve life

display : Dict.Dict (number, number) () -> Int -> Int -> Element
display grid width height = container width height middle (
  collage gridSize gridSize
    ( map (traced (solid black)) (renderGrid gridSize) ++ (renderLife grid) )
  )

main = lift3 display (foldp (nextGeneration) initialGrid (fps 1)) Window.width Window.height