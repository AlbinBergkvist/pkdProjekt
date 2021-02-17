{-Pkd projekt Henrik Jädersten, Nils Hartman, Albin Bergkbist-}

type Board = [(Grid,Square)]

type Grid = (Int,Int)

data Square = Empty | Piece Type Color

data Type = K | Q | R | B | N | P

data Color = Black | White

