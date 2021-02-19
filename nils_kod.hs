{-Pkd projekt Henrik Jädersten, Nils Hartman, Albin Bergkvist-}

type Board = [(Grid,Square)]

type Grid = (Int,Int)

data Square = Empty | Piece Type Color

data Type = K | Q | R | B | N | P

data Color = Black | White


newGame :: Board
newGame = undefined


move :: Grid -> Grid -> Board -> Board
move = undefined


availableSquare :: Square -> Bool
availableSquare Empty = False
availableSquare (Piece _ _) = True


validMove :: Square -> Board -> [Grid]
validMove = undefined


victory :: Board -> Bool
victory = undefined

