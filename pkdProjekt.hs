{-Pkd projekt Henrik Jädersten, Nils Hartman, Albin Bergkvist-}

import Data.List
import Data.Char

type Board = [(Grid,Square)]

type Grid = (Int,Int)

data Square = Empty | Piece Type Color deriving(Eq,Show)

data Type = K | Q | R | B | N | P deriving(Eq,Show)

data Color = Black | White | None deriving(Eq,Show)


{-  newGame
    Function that creates the list of locations for starting a new game
    RETURNS: List of locations and pieces or empty squares
-}
newGame :: Board
newGame = [ ((1,8),(Piece R Black)) , ((2,8),(Piece N Black)) , ((3,8),(Piece B Black)) , ((4,8),(Piece K Black)) , ((5,8),(Piece Q Black)) , ((6,8),(Piece B Black)) , ((7,8),(Piece N Black)) , ((8,8),(Piece R Black)),
            ((1,7),(Piece P Black)) , ((2,7),(Piece P Black)) , ((3,7),(Piece P Black)) , ((4,7),(Piece P Black)) , ((5,7),(Piece P Black)) , ((6,7),(Piece P Black)) , ((7,7),(Piece P Black)) , ((8,7),(Piece P Black)),
            ((1,6),(Empty)) , ((2,6),(Empty)) , ((3,6),(Empty)) , ((4,6),(Empty)) , ((5,6),(Empty)) , ((6,6),(Empty)) , ((7,6),(Empty)) , ((8,6),(Empty)) ,
            ((1,5),(Empty)) , ((2,5),(Empty)) , ((3,5),(Empty)) , ((4,5),(Empty)) , ((5,5),(Empty)) , ((6,5),(Empty)) , ((7,5),(Empty)) , ((8,5),(Empty)) , 
            ((1,4),(Empty)) , ((2,4),(Empty)) , ((3,4),(Empty)) , ((4,4),(Empty)) , ((5,4),(Empty)) , ((6,4),(Empty)) , ((7,4),(Empty)) , ((8,4),(Empty)) , 
            ((1,3),(Empty)) , ((2,3),(Empty)) , ((3,3),(Empty)) , ((4,3),(Empty)) , ((5,3),(Empty)) , ((6,3),(Empty)) , ((7,3),(Empty)) , ((8,3),(Empty)) ,
            ((1,2),(Piece P White)) , ((2,2),(Piece P White)) , ((3,2),(Piece P White)) , ((4,2),(Piece P White)) , ((5,2),(Piece P White)) , ((6,2),(Piece P White)) , ((7,2),(Piece P White)) , ((8,2),(Piece P White)),
            ((1,1),(Piece R White)) , ((2,1),(Piece N White)) , ((3,1),(Piece B White)) , ((4,1),(Piece K White)) , ((5,1),(Piece Q White)) , ((6,1),(Piece B White)) , ((7,1),(Piece N White)) , ((8,1),(Piece R White)) ]


{-  fromBoardtoBoardList board
    converts a board list to a list of boardlists in sets of 8 per list element
    RETURNS: List of boards with 8 elements in each element of the list
    EXAMPLES: fromBoardtoBoardList [((1,6),(Empty)) , ((2,6),(Empty)) , ((3,6),(Empty)) , ((4,6),(Empty)) , ((5,6),(Empty)) , ((6,6),(Empty)) , ((7,6),(Empty)) , ((8,6),(Empty)) , ((1,5),(Empty)) , ((2,5),(Empty)) , ((3,5),(Empty)) , ((4,5),(Empty)) , ((5,5),(Empty)) , ((6,5),(Empty)) , ((7,5),(Empty)) , ((8,5),(Empty))] == [[((1,6),Empty),((2,6),Empty),((3,6),Empty),((4,6),Empty),((5,6),Empty),((6,6),Empty),((7,6),Empty),((8,6),Empty)],[((1,5),Empty),((2,5),Empty),((3,5),Empty),((4,5),Empty),((5,5),Empty),((6,5),Empty),((7,5),Empty),((8,5),Empty)]]
              fromBoardtoBoardList [] = []
-}
fromBoardtoBoardList :: Board -> [Board]
-- VARIANT: length board
fromBoardtoBoardList [] = []
fromBoardtoBoardList board = (take 8 board) : fromBoardtoBoardList (drop 8 board)


{-  printBoard board
    takes a board in list form and prints it as a grid including labeling the x and y axis with corresponding letter and numbers
    SIDE EFFECTS: prints a chessboard with labeled x and y axis
    EXAMPLES:
        Example session:
            > printBoard newGame
            8 r n b k q b n r
            7 p p p p p p p p
            6 - - - - - - - -
            5 - - - - - - - -
            4 - - - - - - - -
            3 - - - - - - - -
            2 P P P P P P P P
            1 R N B K Q B N R
              A B C D E F G H
-}
printBoard :: Board -> IO()
printBoard board = mapM_ putStrLn $ gridNum $ map (intercalate " ") $ icons $ second $ fromBoardtoBoardList board
    where
        second board = map (map snd) board
        icons board = map (map printIcon) board ++ [[" ","A","B","C","D","E","F","G","H"]]
        gridNum [x] = [x]
        gridNum (x:xs) = ((show (length (x:xs) -1) ) ++ " " ++ x) : (gridNum xs)


{-  printIcon piece
    prints the corresponding icon of a given piece
    RETURNS: icon of given piece
    SIDE EFFECTS: icons do not work on windows and are therefore replaced by letters with capital letters representing white 
                    pieces and lower case letters representing the black
    EXAMPLES: printIcon Empty == "-"
              printIcon (Piece B White) == "B"
              printIcon (Piece K Black) == "k"
-}
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


{- PieceMove piece board placement
  lists all the available moves a specific piece have in a specific place on a specific board.
  RETURN: All available grids the piece can move to on given Board.
  EXAMPLES:
  pieceMove (Piece P White) newGame (2,2) == [(2,3),(2,4)]
  pieceMove (Piece K Black) newGame (5,8) == []
-}

pieceMove :: Square -> Board -> Grid -> [Grid]
pieceMove Empty _ _= error "not a piece"
pieceMove (Piece K color) b (x,y) = kingcheck b (x,y) color (validK b color $[(x,y) | x <- [x-1,x,x+1] , y <- [y-1,y,y+1] ]) 

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
validK _ _ [] = []
validK b color (x:xs)   | getColor (snd(findSquare x b)) == color = validK b color xs
                        | otherwise = x : validK b color xs
kingcheck _ _ _ [] = []
kingcheck b current color (x:xs)    | check (move current x b b) color == True = [] ++ kingcheck b current color xs
                                    | otherwise = [x] ++ kingcheck b current color xs
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
                                    else if  y == 7 then if getColor' (x,y-1) b == None then (x,y-1) : pawnMoveStraight b Black (x,y-1) else []
                                    else if getColor' (x,y-1) b == None then (x,y-1) : [] else []
pawnMoveDiagonal b White (x,y) = if getColor' (x+1,y+1) b == Black then [(x+1,y+1)] ++ pawnMoveDiagonal' b White else [] ++ pawnMoveDiagonal' b White
  where 
  pawnMoveDiagonal' b color = if getColor' (x-1,y+1) b == Black then [(x-1,y+1)] else []

pawnMoveDiagonal b Black (x,y) = if getColor' (x+1,y-1) b == White then [(x+1,y-1)] ++ pawnMoveDiagonal' b Black else [] ++ pawnMoveDiagonal' b Black
  where 
  pawnMoveDiagonal' b color = if getColor' (x-1,y-1) b == White then [(x-1,y-1)] else []

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------

findSquare :: Grid -> Board -> (Grid, Square)
findSquare g [b] = b
findSquare g (b:bs) | g == fst b = b
                    | otherwise = findSquare g bs


{-  findSquare' location board
    Shows the status of the given location and whether it is empty or contains a piece
    RETURNS: Tuple with location and type of piece
    EXAMPLES: findSquare' (1,8) newGame == ((1,8),Piece R Black)
              findSquare' (3,4) newGame == ((3,4),Empty)
-}
findSquare' :: Grid -> Board -> (Grid,Square)
findSquare' g b = b !!  (((8-(snd g))*8 + (fst g)) -1)


getColor :: Square -> Color
getColor Empty = None
getColor (Piece _ c) = c


getColor' :: Grid -> Board -> Color
getColor' (x,y) b = getColor (snd (findSquare (x,y) b))



{-  getPiece location board
    retrieves the piece at a given location on a board
    PRE: the given location is on the board
    RETURNS: Piece and its value and color
             Empty if the square is empty
    EXAMPLES: getPiece (8,2) newGame == Piece P White
              getPiece (6,3) newGame == Empty
-}
getPiece :: Grid -> Board -> Square
getPiece grid board = snd $ findSquare' grid board


{-  move piece newPosition board board
    given a piece's grid location, the function will move the piece to the new location and update the board
    PRE: the grid locations are on the board
    RETURNS: updated board with the new location after move
    EXAMPLES: move (2,2) (2,3) newGame newGame = [((1,8),Piece R Black),((2,8),Piece N Black),((3,8),Piece B Black),((4,8),Piece K Black),((5,8),Piece Q Black),((6,8),Piece B Black),((7,8),Piece N Black),((8,8),Piece R Black),((1,7),Piece P Black),((2,7),Piece P Black),((3,7),Piece P Black),((4,7),Piece P Black),((5,7),Piece P Black),((6,7),Piece P Black),((7,7),Piece P Black),((8,7),Piece P Black),((1,6),Empty),((2,6),Empty),((3,6),Empty),((4,6),Empty),((5,6),Empty),((6,6),Empty),((7,6),Empty),((8,6),Empty),((1,5),Empty),((2,5),Empty),((3,5),Empty),((4,5),Empty),((5,5),Empty),((6,5),Empty),((7,5),Empty),((8,5),Empty),((1,4),Empty),((2,4),Empty),((3,4),Empty),((4,4),Empty),((5,4),Empty),((6,4),Empty),((7,4),Empty),((8,4),Empty),((1,3),Empty),((2,3),Piece P White),((3,3),Empty),((4,3),Empty),((5,3),Empty),((6,3),Empty),((7,3),Empty),((8,3),Empty),((1,2),Piece P White),((2,2),Empty),((3,2),Piece P White),((4,2),Piece P White),((5,2),Piece P White),((6,2),Piece P White),((7,2),Piece P White),((8,2),Piece P White),((1,1),Piece R White),((2,1),Piece N White),((3,1),Piece B White),((4,1),Piece K White),((5,1),Piece Q White),((6,1),Piece B White),((7,1),Piece N White),((8,1),Piece R White)]
-}
move :: Grid -> Grid -> Board -> Board -> Board
-- VARIANT: length board
move _ _ _ [] = []
move currentP newP refBoard (b:bs) | (fst b) == currentP = (currentP,Empty) : move currentP newP refBoard bs
                                   | (fst b) == newP     = (newP, (snd (findSquare' currentP refBoard))) : move currentP newP refBoard bs
                                   | otherwise           = b : move currentP newP refBoard bs


{-  availableSquare square
    checks if the given square is empty or if it contains a piece
    RETURNS: True iff there is a piece on the given square
             False iff the given square is empty
    EXAMPLES: availableSquare Empty == False
              availableSquare (Piece K Black) == True
-}
availableSquare :: Square -> Bool
availableSquare Empty = False
availableSquare (Piece _ _) = True


{-  inputToGrid input
    converts a text input to a grid location
    PRE: first letter is a letter A-F
    RETURNS: Grid location representing the text input
    EXAMPLES: inputToGrid "a2" == (1,2)
              inputToGrid "C4" == (3,4)
              inputToGrid "H8" == (8,8)
-}
inputToGrid :: String -> Grid
inputToGrid (x:y:_)
    | x == 'a' || x == 'A' = (1,digitToInt y)
    | x == 'b' || x == 'B' = (2,digitToInt y)
    | x == 'c' || x == 'C' = (3,digitToInt y)
    | x == 'd' || x == 'D' = (4,digitToInt y)
    | x == 'e' || x == 'E' = (5,digitToInt y)
    | x == 'f' || x == 'F' = (6,digitToInt y)
    | x == 'g' || x == 'G' = (7,digitToInt y)
    | x == 'h' || x == 'H' = (8,digitToInt y)


{-  isWhite piece
    checks if given piece is a white piece
    RETURNS: True iff piece is white
             False if piece is black or square is empty
    EXAMPLES: isWhite (Piece Q White) == True
              isWhite (Piece B Black) == False
              isWhite Empty == False
-}
isWhite :: Square -> Bool
isWhite (Piece _ White) = True
isWhite  _ = False


{-  isBlack piece
    checks if given piece is a black piece
    RETURNS: True iff piece is black
             False if piece is white or the square is empty
    EXAMPLES: isBlack (Piece P Black) == True
              isBlack (Piece N White) == False
              isBlack Empty == False
-}
isBlack :: Square -> Bool
isBlack (Piece _ Black) = True
isBlack _ = False


{-  play color boardState
    function that runs through the set of rules for the chess game along with moving pieces according to the specific rules for the color and the boardstate
    SIDE EFFECTS: keeps playing the game until a player has won
    EXAMPLES: 
        Example session:
            > play White newGame
            8 r n b k q b n r
            7 p p p p p p p p
            6 - - - - - - - -
            5 - - - - - - - -
            4 - - - - - - - -
            3 - - - - - - - -
            2 P P P P P P P P
            1 R N B K Q B N R
            A B C D E F G H

            White's turn. Please select a piece to move:
            > c2
            Select where to place the piece:
            > c4
            8 r n b k q b n r
            7 p p p p p p p p
            6 - - - - - - - -
            5 - - - - - - - -
            4 - - P - - - - -
            3 - - - - - - - -
            2 P P - P P P P P
            1 R N B K Q B N R
            A B C D E F G H

            Black's turn. Please select a piece to move:
-}
play :: Color -> Board -> IO ()
play color boardState = do
    putStrLn ""
    printBoard boardState
    p <- choosePiece color boardState
    let piece = p
    np <- chooseMove piece boardState
    let newPosition = np
        updatedBoard = move piece newPosition boardState boardState
    if victory boardState color == True
        then do
            putStrLn ""
            putStrLn $ printColor color ++ " has won the game. Congratulations!"
            putStrLn ""
            putStrLn "Do you want to play again? (Yes/No)"
            answer <- getLine
            if answer == "yes" || answer == "Yes"
                then do
                    play White newGame
                else do
                    putStrLn ""
                    putStrLn "Thank you for playing!"
        else do 
            if color == White
                then
                    play Black updatedBoard
                else
                    play White updatedBoard
    where
        printColor White = "White"
        printColor Black = "Black"


{-  chooseMove piece boardState
    allows player to choose where to place the piece given a set of rules that apply to the color and boardstate
    RETURNS: grid location
    SIDE EFFECTS: Asks player to input desired loaction for the chosen piece
    EXAMPLES: 
        Example session:
            > chooseMove (2,2) newGame
            Select where to place the piece:
            > b3
            (2,3)
-}
chooseMove :: Grid -> Board -> IO Grid
chooseMove piece boardState = do
    putStrLn ""
    putStrLn "Select where to place the piece:"
    n <- getLine
    let newPosition = inputToGrid n
    if elem newPosition (pieceMove (getPiece piece boardState) boardState piece) == False
        then do
            putStrLn ""
            putStrLn "Not a valid move for chosen piece. Please select a new move"
            chooseMove piece boardState
        else do
            return newPosition
    

{-  choosePiece color boardstate
    allows user to select which piece to move by typing the location of the piece in the boardstate and following a given set of rules according to color and boardstate
    RETURNS: grid location
    SIDE EFFECTS: asks player to input desired move
    EXAMPLES: 
        Example session:
            > choosePiece White newGame
            White's turn. PLease select a piece to move:
            > c3
            You have chosen an empty square. Please select a new piece.
            White's turn. Please select a piece to move:
            > d2
            (4,2)
-} 
choosePiece :: Color -> Board -> IO Grid
choosePiece color boardState = do
    if color == White
        then do
            putStrLn ""
            putStrLn "White's turn. Please select a piece to move:"
        else do
            putStrLn ""
            putStrLn "Black's turn. Please select a piece to move:"
    p <- getLine
    if validInput p == False
        then do putStrLn ""
                putStrLn "Invalid input. Must be letter A-H and number 1-8 in form letter+number ex. b3 or E5"
                choosePiece color boardState
        else do
            let piece = inputToGrid p
            if getPiece piece boardState == Empty
                then do
                    putStrLn ""
                    putStrLn "You have chosen an empty square. Please select a new piece."
                    choosePiece color boardState
                else do
                    if (pieceMove (getPiece piece boardState) boardState piece) == []
                        then do
                            putStrLn ""
                            putStrLn "No available moves for chosen piece. Please select a new piece."
                            choosePiece color boardState
                        else do
                            if color == White
                                then do
                                    if (isWhite $ snd $ findSquare' piece boardState) == False
                                        then do
                                            let grid = (toUpper $ head p) : tail p
                                            putStrLn ""
                                            putStrLn $ "No white piece at " ++ grid ++ ". Please select a new piece to move."
                                            choosePiece White boardState
                                        else do
                                            return piece
                                else do
                                    if (isBlack $ snd $ findSquare' piece boardState) == False
                                        then do
                                            let grid = (toUpper $ head p) : tail p
                                            putStrLn ""
                                            putStrLn $ "No black piece at " ++ grid ++ ". Please select a new piece to move."
                                            choosePiece Black boardState
                                        else do
                                            return piece


{-  validInput input
    checks if the given input is a correctly formated input
    RETURNS: True iff input is correctly formated
             False if the input is empty or wrongly formated
    EXAMPLES: validInput "" == False
              validInput "a3" == True
              validInput "F7" == True
              validInput "f" == False
              validInput "a9" == False
-}
validInput :: String -> Bool
validInput [] = False
validInput [_] = False
validInput (x:y:z) | (elem x ['A'..'H'] || elem x ['a'..'h']) && elem y ['1'..'8'] && z == [] = True
                   | otherwise = False






--xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx Albins Kod

{-Här måste avMoves bytas ut till en lista med tillgängliga moves (som är legal) possibly behövs
det skiljas på ens egna moves och motståndarens, men det bör lätt att se när vi väl implementerar
det behövs även en funktion för att göra en lista med alla pjäser som fortfarande är i spel för
detta att fungera  -}

victory :: Board -> Color -> Bool
victory board White = if (check board Black) == True && (listToMoves board (listPieces board White)) == [] then True else False
victory board Black = if (check board White) == True && (listToMoves board (listPieces board Black)) == [] then True else False

draw :: Board -> Color -> Bool
draw board White = if (check board White) == False && (listToMoves board (listPieces board Black)) == [] then True else False
draw board Black = if (check board Black) == False && (listToMoves board (listPieces board White)) == [] then True else False





check :: Board -> Color -> Bool
check board f = eqMoves (kingFinder board f) (listToMoves board (listPieces board f))

--andra board är listan med pjäser
listToMoves :: Board -> Board -> [Grid]
listToMoves board [] = []
listToMoves board (x:xs) = pieceMove (snd x) board (fst x) ++ listToMoves board xs

eqMoves :: Grid -> [Grid] -> Bool
eqMoves king [] = False
eqMoves king (x:xs) = if king == x then True else eqMoves king xs


kingFinder :: Board -> Color -> Grid
kingFinder ((t,Piece K White):xs) White = t
kingFinder ((t,Piece K Black):xs) Black = t
kingFinder (_:xs) f = kingFinder xs f
        

listPieces :: Board -> Color -> Board --ska användas för att få vilka pieces som validmoves ska använda
listPieces [] _ = []                 --skulle kunna göras till color specific
listPieces ((g,Piece t White):xs) White = listPieces xs White
listPieces ((g,Piece t Black):xs) Black = listPieces xs Black
listPieces ((g,Piece t c):xs) f = (g,Piece t c) : listPieces xs f
listPieces (_:xs) f = listPieces xs f

-- xxxxxxxxxxxxxxxxxxxxxxxxxxxxx







{-  main
    main game function that welcomes player and starts the game process
    SIDE EFFECTS: welcomes players and begins play of game
    EXAMPLES:
        Example session:
            > main

            Hello! Let's play chess

            Players will take turns moving pieces
            To move a piece, select which piece accoridng to their location on the grid
            by specifying the letter and number in the form x# or X#. Then select where
            to place the piece on the board in the same way by choosing a grid location.

            Good luck!

            8 r n b k q b n r
            7 p p p p p p p p
            6 - - - - - - - -
            5 - - - - - - - -
            4 - - - - - - - -
            3 - - - - - - - -
            2 P P P P P P P P
            1 R N B K Q B N R
            A B C D E F G H

            White's turn. Please select a piece to move:
-}
main :: IO ()
main = do
    putStrLn ""
    putStrLn "Hello! Let's play chess"
    putStrLn ""
    putStrLn "Players will take turns moving pieces"
    putStrLn "To move a piece, select which piece accoridng to their location on the grid"
    putStrLn "by specifying the letter and number in the form x# or X#. Then select where"
    putStrLn "to place the piece on the board in the same way by choosing a grid location."
    putStrLn ""
    putStrLn "Good luck!"
    play White newGame