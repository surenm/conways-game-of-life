import Dict
import Graphics.Collage
import Window
import Debug

cellSize : number
cellSize = 20

gridSize : number
gridSize = 500

funColor: Color
funColor = rgba 81 116 22 1.0

hasLiveCell : Dict.Dict (Int, Int) () -> (Int, Int) -> Bool
hasLiveCell grid position = Dict.member position grid
initialGrid: Dict.Dict (number, number) ()
initialGrid = (Dict.fromList [((1,0), ()), ((2, 1), ()), ((0, 2), ()), ((1, 2), ()), ((2, 2), ())])

verticalLine : Float -> Float -> Path
verticalLine height ordinate = path [ (ordinate, -height/2), (ordinate, height/2) ]

horizontalLine : Float -> Float -> Path
horizontalLine width abscissca = path [ (-width/2, abscissca), (width/2, abscissca) ]

generateGrid : Float -> [Path]
generateGrid size =
  let verticalLines = map (verticalLine size) (map (\n -> n * cellSize) [(-size/(2 * cellSize))..(size/(2 * cellSize))])
      horizontalLines = map (horizontalLine size) (map (\n -> n * cellSize) [(-size/(2 * cellSize))..(size/(2 * cellSize))]) in 

    horizontalLines ++ verticalLines

generateCell : (Float, Float) -> Form
generateCell (x, y) = move ((-gridSize/2 + x + cellSize/2, gridSize/2 - y - cellSize/2)) (filled funColor (circle (cellSize/2.5)))

generateCells : [Form]
generateCells = map generateCell (map (\(x,y) -> (x * cellSize, y * cellSize)) (Dict.keys grid))
--generateCells = [move(-225, 225) (filled funColor (circle 25))]

display : Int -> Int -> Element
display width height = container width height middle (
                        collage gridSize gridSize
                          ( map (traced (solid black)) (generateGrid gridSize) ++ generateCells )
                        )

main: Signal Element
main = lift2 display Window.width Window.height