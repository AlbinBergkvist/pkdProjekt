{-Pkd projekt Henrik Jädersten, Nils Hartman, Albin Bergkvist-}

import Data.List

type Board = [[(Grid,Square)]] 

type Grid = (Int,Int)

data Square = Empty | Piece Type Color deriving(Eq,Show)

data Type = K | Q | R | B | N | P deriving(Eq,Show)

data Color = Black | White deriving(Eq,Show)

play :: IO()
play = undefined


newGame :: Board
newGame = [ [((1,8),(Piece R Black)) , ((2,8),(Piece N Black)) , ((3,8),(Piece B Black)) , ((4,8),(Piece K Black)) , ((5,8),(Piece Q Black)) , ((6,8),(Piece B Black)) , ((7,8),(Piece N Black)) , ((8,8),(Piece R Black))],
            [((1,7),(Piece P Black)) , ((2,7),(Piece P Black)) , ((3,7),(Piece P Black)) , ((4,7),(Piece P Black)) , ((5,7),(Piece P Black)) , ((6,7),(Piece P Black)) , ((7,7),(Piece P Black)) , ((8,7),(Piece P Black))],
            [((1,6),(Empty)) , ((2,6),(Empty)) , ((3,6),(Empty)) , ((4,6),(Empty)) , ((5,6),(Empty)) , ((6,6),(Empty)) , ((7,6),(Empty)) , ((8,6),(Empty)) ],
            [((1,5),(Empty)) , ((2,5),(Empty)) , ((3,5),(Empty)) , ((4,5),(Empty)) , ((5,5),(Empty)) , ((6,5),(Empty)) , ((7,5),(Empty)) , ((8,5),(Empty)) ], 
            [((1,4),(Empty)) , ((2,4),(Empty)) , ((3,4),(Empty)) , ((4,4),(Empty)) , ((5,4),(Empty)) , ((6,4),(Empty)) , ((7,4),(Empty)) , ((8,4),(Empty)) ], 
            [((1,3),(Empty)) , ((2,3),(Empty)) , ((3,3),(Empty)) , ((4,3),(Empty)) , ((5,3),(Empty)) , ((6,3),(Empty)) , ((7,3),(Empty)) , ((8,3),(Empty)) ],
            [((1,2),(Piece P White)) , ((2,2),(Piece P White)) , ((3,2),(Piece P White)) , ((4,2),(Piece P White)) , ((5,2),(Piece P White)) , ((6,2),(Piece P White)) , ((7,2),(Piece P White)) , ((8,2),(Piece P White))],
            [((1,1),(Piece R White)) , ((2,1),(Piece N White)) , ((3,1),(Piece B White)) , ((4,1),(Piece K White)) , ((5,1),(Piece Q White)) , ((6,1),(Piece B White)) , ((7,1),(Piece N White)) , ((8,1),(Piece R White))] ]


printBoard :: Board -> IO()
printBoard board = mapM_ putStrLn $ gridNum $ map (intercalate " ") $ icons $ second board
    where
        second board = map (map snd) board
        icons board = map (map printIcon) board ++ [[" ","A","B","C","D","E","F","G","H"]]
        gridNum [x] = [x]
        gridNum (x:xs) = ((show (length (x:xs) -1) ) ++ " " ++ x) : (gridNum xs)


printIcon :: Square -> String
printIcon Empty = "+" --"□"
printIcon (Piece K White) = "♚"
printIcon (Piece Q White) = "♛"
printIcon (Piece R White) = "♜"
printIcon (Piece B White) = "♝"
printIcon (Piece N White) = "♞"
printIcon (Piece P White) = "♟"
printIcon (Piece K Black) = "♔"
printIcon (Piece Q Black) = "♕"
printIcon (Piece R Black) = "♖"
printIcon (Piece B Black) = "♗"
printIcon (Piece N Black) = "♘"
printIcon (Piece P Black) = "♙"


move :: Grid -> Grid -> Board -> Board
move = undefined


availableSquare :: Square -> Bool
availableSquare Empty = False
availableSquare (Piece _ _) = True


validMove :: Square -> Board -> [Grid]
validMove = undefined


victory :: Board -> Bool
victory = undefined