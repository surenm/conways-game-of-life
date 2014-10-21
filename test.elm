cellSize : number
cellSize = 20

gridSize : number
gridSize = 500

validCell: (Int, Int) -> Bool
validCell (x, y) = x >= 0 && y >= 0 && x < gridSize/cellSize && y < gridSize/cellSize

neighbouringCells: (Int, Int) -> [(Int, Int)]
neighbouringCells position =
  let deltas = [(1, 0), (1, -1), (0, -1), (-1, -1), (-1, 0), (-1, 1), (0, 1), (1, 1)] in
    if validCell(position) 
      then filter validCell (map (\delta -> add position delta) deltas)
    else
      []

add: (number, number) -> (number, number) -> (number, number)
add (a1, b1) (a2, b2) = (a1 + a2, b1 + b2)

main = asText (neighbouringCells (24, 24))