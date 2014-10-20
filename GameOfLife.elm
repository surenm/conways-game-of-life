import Dict
import Graphics.Collage
import Window

cellSize : number
cellSize = 50

gridSize : number
gridSize = 500

grid = (Dict.fromList [((0,0), ()), ((0, 1), ())])

hasLiveCell : Dict.Dict (Int, Int) () -> (Int, Int) -> Bool
hasLiveCell grid position = Dict.member position grid

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
generateCell position = move position (filled black (circle 50))

display : Int -> Int -> Element
display width height = container width height middle (
                        collage gridSize gridSize
                          (map (traced (solid black)) (generateGrid gridSize))
                        )

main : Signal Element
main = lift2 display Window.width Window.height
