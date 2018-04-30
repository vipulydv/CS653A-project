import Data.List
import Text.Read
import System.Exit
import Debug.Trace

emptyBoard = "         "

type Board = String
type Position = Int
type Score = Int

data Tree a = Node a [Tree a]  deriving Show

main = do
    putStrLn ("\nWelcome to Tic-Tac-Toe.\n")
    putStrLn ("You go first and to play your move enter a cell number between 0 and 8. The cell numbers are shown below:\n")
    putStrLn (" " ++ "0" ++ " | " ++ "1" ++ " | " ++ "2" ++ " \n--- --- ---\n " ++ "3" ++ " | " ++ "4" ++ " | " ++ "5" ++ " \n--- --- ---\n " ++ "6" ++ " | " ++ "7" ++ " | " ++ "8" ++ " \n")
    putStrLn("\nPlease note that your move is represented by X and that of computer by O.\n\n\n")
    play emptyBoard

play :: Board -> IO ()
play board = do
    putStrLn (printBoard board)
    if (isWinBoard board) == True then do
        putStrLn ("Player: " ++ (getCurrSymb board) ++ " wins.\n")
        putStrLn ("Do you want to play another game? [y/n]\n")
        myInput <- getLine
        if myInput == "y" then
            play emptyBoard
            else if myInput =="n" then
                exitSuccess 
                else do
                    putStr ("Please enter \'y\' for yes and \'n\' for no :\n")
                    play board
    else
        putStr "Enter your move : "
    input <- getLine
    let pos = (readMaybe input :: Maybe Int) -- error on entering an invalid move
    if pos == Nothing then do
        putStrLn ("Please enter a valid move.\n")
        play board
    else do
        let Just newpos = pos
        if (isValidMove board newpos) == True then do
            let afterPlayerBoard = insertMove board newpos
            if (isMoveLeft afterPlayerBoard) == True then
                play (nextMove afterPlayerBoard)
            else do
                putStrLn ("It's a TIE.\n")
                putStrLn ("Do you want to play another game? [y/n]\n")
                myInput <- getLine
                if myInput == "y" then
                    play emptyBoard
                    else if myInput == "n" then
                        exitSuccess 
                        else do
                            putStr ("Please enter \'y\' for yes and \'n\' for no :\n")
                            play board
        else do
            putStrLn ("Please enter a valid move.\n")
            play board

printBoard :: Board -> String
printBoard board = " " ++ [board!!0] ++ " | " ++ [board!!1] ++ " | " ++ [board!!2] ++ " \n--- --- ---\n " ++ [board!!3] ++ " | " ++ [board!!4] ++ " | " ++ [board!!5] ++ " \n--- --- ---\n " ++ [board!!6] ++ " | " ++ [board!!7] ++ " | " ++ [board!!8] ++ " \n"

getCurrSymb :: Board -> String
getCurrSymb board
    | getNextSymb board == "O" = "X"
    | getNextSymb board == "X" = "O"
    | otherwise                = error "How did you come here?"

getNextSymb :: Board -> String
getNextSymb board = if (length (filter (== 'X') board)) == (length (filter (== 'O') board)) then
                        "X"
                    else
                        "O"

isValidMove :: Board -> Position -> Bool
isValidMove board pos = afterMove `elem` allowedMoves
    where afterMove    = insertMove board pos
          allowedMoves = getAllPossibleBoards board

insertMove :: Board -> Position -> Board
insertMove board position = replace board (getNextSymb board) position

replace :: Board ->  String -> Position -> Board
replace board char pos = take pos board ++ char ++ drop (pos+1) board

getAllPossibleBoards :: Board -> [Board]
getAllPossibleBoards board
    | isWinBoard board = []
    | otherwise        = [replace board (getNextSymb board) i | i<-[0..8], board!!i == ' ']

isWinBoard :: Board -> Bool
isWinBoard board = any isWinningTriple (getTriples board)

isWinningTriple :: String -> Bool
isWinningTriple triple = if (triple=="XXX" || triple=="OOO") == True then
                            True
                         else
                            False

getTriples :: Board -> [String]
getTriples board = [takeRange board [0,1,2]] ++ [takeRange board [3,4,5]] ++ [takeRange board [6,7,8]] ++ [takeRange board [0,3,6]] ++ [takeRange board [1,4,7]] ++ [takeRange board [2,5,8]] ++ [takeRange board [0,4,8]] ++ [takeRange board [2,4,6]]

takeRange :: [t] -> [Int] -> [t]
takeRange board range = [board!!i | i<-range]

isMoveLeft :: Board -> Bool
isMoveLeft a = if (length $ filter (== ' ') a) == 0 then
                    False
                else
                    True

nextMove :: Board -> Board
nextMove board = applyNextMove (expandGameTree board)

applyNextMove :: Tree Board -> Board
applyNextMove (Node x (y:ys)) = getBoardFromTree ((y:ys)!!(maxIndex (scores (y:ys))))
    where maxIndex xs = head (filter ((== maximum xs) . (xs!!)) [0..]) 
          scores (x:xs) = [getStateScore y | y <-(x:xs)]

getBoardFromTree :: Tree t -> t
getBoardFromTree (Node a _) = a

getStateScore :: Tree Board -> Score
getStateScore (Node x [])     = getScore x
getStateScore (Node x (y:ys)) = (minimax x) [getStateScore z | z<-(y:ys)]

getScore :: Board -> Score
getScore board
    | (isWinBoard board) && ((getNextSymb board) == "X") = 10
    | (isWinBoard board) && ((getNextSymb board) == "O") = -10
    | otherwise                                          = 0

expandGameTree :: Board -> Tree Board
expandGameTree board = Node board $ successors board
    where successors state = [Node s (successors s) | s <- getAllPossibleBoards state, s/=[]]

minimax :: (Ord a, Foldable t) => Board -> t a -> a
minimax board
    | getNextSymb board == "X" = minimum
    | getNextSymb board == "O" = maximum