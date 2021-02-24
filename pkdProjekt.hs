{-Pkd projekt Henrik Jädersten, Nils Hartman, Albin Bergkvist-}

import Data.List
import Data.Char

type Board = [(Grid,Square)]

type Grid = (Int,Int)

data Square = Empty | Piece Type Color deriving(Eq,Show)

data Type = K | Q | R | B | N | P deriving(Eq,Show)

data Color = Black | White | None deriving(Eq,Show)




newGame :: Board
newGame = [ ((1,8),(Piece R Black)) , ((2,8),(Piece N Black)) , ((3,8),(Piece B Black)) , ((4,8),(Piece K Black)) , ((5,8),(Piece Q Black)) , ((6,8),(Piece B Black)) , ((7,8),(Piece N Black)) , ((8,8),(Piece R Black)),
            ((1,7),(Piece P Black)) , ((2,7),(Piece P Black)) , ((3,7),(Piece P Black)) , ((4,7),(Piece P Black)) , ((5,7),(Piece P Black)) , ((6,7),(Piece P Black)) , ((7,7),(Piece P Black)) , ((8,7),(Piece P Black)),
            ((1,6),(Empty)) , ((2,6),(Empty)) , ((3,6),(Empty)) , ((4,6),(Empty)) , ((5,6),(Empty)) , ((6,6),(Empty)) , ((7,6),(Empty)) , ((8,6),(Empty)) ,
            ((1,5),(Empty)) , ((2,5),(Empty)) , ((3,5),(Empty)) , ((4,5),(Empty)) , ((5,5),(Empty)) , ((6,5),(Empty)) , ((7,5),(Empty)) , ((8,5),(Empty)) , 
            ((1,4),(Empty)) , ((2,4),(Empty)) , ((3,4),(Empty)) , ((4,4),(Empty)) , ((5,4),(Empty)) , ((6,4),(Empty)) , ((7,4),(Empty)) , ((8,4),(Empty)) , 
            ((1,3),(Empty)) , ((2,3),(Empty)) , ((3,3),(Empty)) , ((4,3),(Empty)) , ((5,3),(Empty)) , ((6,3),(Empty)) , ((7,3),(Empty)) , ((8,3),(Empty)) ,
            ((1,2),(Piece P White)) , ((2,2),(Piece P White)) , ((3,2),(Piece P White)) , ((4,2),(Piece P White)) , ((5,2),(Piece P White)) , ((6,2),(Piece P White)) , ((7,2),(Piece P White)) , ((8,2),(Piece P White)),
            ((1,1),(Piece R White)) , ((2,1),(Piece N White)) , ((3,1),(Piece B White)) , ((4,1),(Piece K White)) , ((5,1),(Piece Q White)) , ((6,1),(Piece B White)) , ((7,1),(Piece N White)) , ((8,1),(Piece R White)) ]

fromBoardtoBoardList :: Board -> [Board]
fromBoardtoBoardList [] = []
fromBoardtoBoardList board = (take 8 board) : fromBoardtoBoardList (drop 8 board)

printBoard :: Board -> IO()
printBoard board = mapM_ putStrLn $ gridNum $ map (intercalate " ") $ icons $ second $ fromBoardtoBoardList board
    where
        second board = map (map snd) board
        icons board = map (map printIcon) board ++ [[" ","A","B","C","D","E","F","G","H"]]
        gridNum [x] = [x]
        gridNum (x:xs) = ((show (length (x:xs) -1) ) ++ " " ++ x) : (gridNum xs)


printIcon :: Square -> String
printIcon Empty = "-" --"□"
printIcon (Piece K White) = "K" --"♚"
printIcon (Piece Q White) = "Q" --"♛"
printIcon (Piece R White) = "R" --"♜"
printIcon (Piece B White) = "B" --"♝"
printIcon (Piece N White) = "N" --"♞"
printIcon (Piece P White) = "P" --"♟"
printIcon (Piece K Black) = "k" --"♔"
printIcon (Piece Q Black) = "q" --"♕"
printIcon (Piece R Black) = "r" --"♖"
printIcon (Piece B Black) = "b" --"♗"
printIcon (Piece N Black) = "n" --"♘"
printIcon (Piece P Black) = "p" --"♙"




-- Nils är snart klar med denna del
pieceMove :: Square -> Board -> Grid -> [Grid]
pieceMove Empty _ _= error "not a piece"
pieceMove (Piece K color) b (x,y) = validK b color $[(x,y) | x <- [x-1,x,x+1] , y <- [y-1,y,y+1] ]
                                where 
                                    validK :: Board -> Color -> [Grid] -> [Grid]
                                    validK _ _ [] = []
                                    validK b color (x:xs)   | getColor (snd(findSquare x b)) == color = validK b color xs
                                                            | otherwise = x : validK b color xs
pieceMove (Piece Q color) b (x,y) =     right b color (x,y) ++
                                        left b color (x,y) ++
                                        up b color (x,y) ++
                                        down b color (x,y) ++ 
                                        downLeft b color (x,y) ++ 
                                        downRight b color (x,y) ++ 
                                        upLeft b color (x,y) ++ 
                                        upRight b color (x,y)     

pieceMove (Piece B color) b (x,y) =     downLeft b color (x,y) ++
                                        downRight b color (x,y) ++ 
                                        upLeft b color (x,y) ++ 
                                        upRight b color (x,y)

pieceMove (Piece R color) b (x,y) =     right b color (x,y) ++ 
                                        left b color (x,y) ++ 
                                        up b color (x,y) ++ 
                                        down b color (x,y)         

pieceMove (Piece N color) b (x,y) = knightMoves b color (x,y)
pieceMove (Piece P color) b (x,y) = pawnMoveDiagonal b color (x,y) ++
                                    pawnMoveStraight b color (x,y)

--------------------------------------------------------------{-All the moves-}------------------------------------------------------------------------------------------
right b color (x,y) = if getColor' (x+1,y) b  == color then [] else if getColor' (x+1,y) b == None then (x+1,y) : right b color (x+1 ,y)    else [(x+1,y)]
left b color (x,y) = if getColor' (x-1,y) b == color then [] else if getColor' (x-1,y) b == None then  (x-1,y) : left b color (x-1 ,y)        else [(x-1,y)]
up b color (x,y) = if getColor' (x,y+1) b == color then [] else if getColor' (x,y+1) b == None then (x, y+1) : up b color (x, y+1)          else [(x,y+1)]
down b color (x,y) = if getColor' (x,y-1) b == color then [] else if getColor' (x,y-1) b == None then (x, y-1) : down b color (x ,y-1)       else [(x,y-1)]
downLeft b color (x,y) = if getColor'(x-1 , y-1) b == color then [] else if getColor' (x-1,y-1) b == None then (x-1 , y-1) : downLeft b color (x-1 , y-1) else [(x-1,y-1)]
downRight b color (x,y) = if getColor' (x-1,y+1) b == color then [] else if getColor' (x-1,y+1) b == None then (x-1,y+1) : downRight b color (x-1 ,y+1) else [(x-1,y+1)]
upLeft b color (x,y) = if getColor' (x+1,y-1) b == color then [] else if getColor' (x+1,y-1) b == None then (x+1,y-1) : upLeft b color (x+1 ,y-1) else [(x+1,y-1)]
upRight b color (x,y) = if getColor' (x+1,y+1) b  == color then [] else if getColor' (x+1,y+1) b == None then (x+1,y+1) : right b color (x+1 ,y+1) else [(x+1,y+1)]
knightMoves b color (x,y) = knightMoves' b color (x,y) knightList 
    where 
        knightMoves' _ _ _ [] = []
        knightMoves' b color (x,y) (l:ls) = if getColor' (x+ (fst l) , y+ (snd l)) b == color then [] ++ knightMoves' b color (x,y) ls else [(x+ (fst l) , y+ (snd l))] ++ knightMoves' b color (x,y) ls
        knightList = [((-1),2), (1,2) , (2,1) , (2,(-1)) , (1,(-2)) , ((-1),(-2)) , ((-2),(-1)) , ((-2),1)]
pawnMoveStraight b color (x,y) =    if color == White then if y == 2 then if getColor' (x,y+1) b == None then (x,y+1) : pawnMoveStraight b White (x,y+1) else []
                                    else if getColor' (x,y+1) b == None then (x,y+1) : [] else []
                                    else if  y == 7 then if getColor' (x,y-1) b == None then (x,y-1) : pawnMoveStraight b White (x,y-1) else []
                                    else if getColor' (x,y-1) b == None then (x,y-1) : [] else []
pawnMoveDiagonal b color (x,y) = if color == White then 
                                    if getColor' (x+1,y+1) b == Black then if getColor' (x-1,y+1) b == Black then (x+1,y+1) : (x-1,y+1) : [] else (x+1,y+1) : []
                                    else if getColor' (x-1,y+1) b == Black then if getColor' (x+1,y+1) b == Black then (x-1,y+1) : (x+1,y+1) : [] else (x-1,y+1) : []
                                    else []
                                    else      
                                    if getColor' (x-1,y-1) b == White then if getColor' (x+1,y-1) b == White then (x+1,y-1) : (x-1,y-1) : [] else (x-1,y-1) : []
                                    else if getColor' (x+1,y-1) b == White then if getColor' (x-1,y-1) b == White then (x+1,y-1) : (x-1,y-1) : [] else (x+1,y-1) : []
                                    else []
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------



findSquare :: Grid -> Board -> (Grid, Square)
findSquare g [b] = b
findSquare g (b:bs) | g == fst b = b
                    | otherwise = findSquare g bs


findSquare' :: Grid -> Board -> (Grid,Square)
findSquare' g b = b !!  (((8-(snd g))*8 + (fst g)) -1)

getColor :: Square -> Color
getColor Empty = None
getColor (Piece _ c) = c

getColor' :: Grid -> Board -> Color
getColor' (x,y) b = getColor (snd (findSquare (x,y) b))

--Inte än gjorda funktioner
move :: Grid -> Grid -> Board -> Board -> Board
move _ _ _ [] = []
move currentP newP refBoard (b:bs) | (fst b) == currentP = (currentP,Empty) : move currentP newP refBoard bs
                                   | (fst b) == newP     = (newP, (snd (findSquare' currentP refBoard))) : move currentP newP refBoard bs
                                   | otherwise           = b : move currentP newP refBoard bs


availableSquare :: Square -> Bool
availableSquare Empty = False
availableSquare (Piece _ _) = True


validMove :: Square -> Board -> [Grid]
validMove = undefined


victory :: Board -> Bool
victory = undefined


play :: IO()
play = undefined
