G52AFP Coursework 1 - Connect 4 Game
   
Shuchi Zhang
scysz2@nottingham.ac.uk
----------------------------------------------------------------------

For flexibility, we define constants for the row and column size of the
board, length of a winning sequence, and search depth for the game tree:

> import Data.List
> import Data.Maybe

> rows :: Int
> rows = 6
>
> cols :: Int
> cols = 7
>
> win :: Int
> win = 4
>
> depth :: Int
> depth = 4

The board itself is represented as a list of rows, where each row is
a list of player values, subject to the above row and column sizes:

> type Board = [Row]
>
> type Row = [Player]

In turn, a player value is either a nought, a blank, or a cross, with
a blank representing a position on the board that is not yet occupied:

> data Player = O | B | X
>               deriving (Ord, Eq, Show)

> gameBoard :: Board
> gameBoard = replicate rows (replicate cols B)

The following code displays a board on the screen:

> showBoard :: Board -> IO ()
> showBoard b = putStrLn (unlines (map showRow b ++ [line] ++ [nums]))
>               where
>                  showRow = map showPlayer
>                  line    = replicate cols '-'
>                  nums    = take cols ['0'..]
>
> showPlayer :: Player -> Char
> showPlayer O = 'O'
> showPlayer B = '.'
> showPlayer X = 'X'

> currentPlayer :: Player
> currentPlayer = O

> main :: IO()
> main = do
>   makeNextMove gameBoard currentPlayer
>   return()

CurrentPlayer make the move, change board and current role of valid, otherwise ask for move again

> makeNextMove :: Board -> Player -> IO()
> makeNextMove cb cp = do
>       showBoard cb
>       tmpBoard <- getInput cb cp 
>       let r = gameOver tmpBoard cp
>       if not (r == Und) 
>           then do
>               putStrLn $ (showResult r) ++ [showPlayer cp]
>               showBoard tmpBoard
>           else makeNextMove tmpBoard (nextPlayer cp)

Check whether is robot or user, get corresponding input

> getInput :: Board -> Player -> IO Board
> getInput cb cp | cp == O = do 
>                   putStrLn $ "Hurry Up Human, make the move: "
>                   getUserInput cb cp
>                | otherwise = do
>                   putStrLn $ "AI is thinking ..."
>                   return( getRobotInput cb cp)

Switch current player

> nextPlayer :: Player -> Player
> nextPlayer X = O
> nextPlayer O = X
> nextPlayer _ = B

Get userInput and examine its validation

> getUserInput :: Board -> Player -> IO Board
> getUserInput cb cp = do 
>       input <- getLine
>       let x = (read input :: Int)
>       let tmpBoard = move x cb cp
>       if (isNothing tmpBoard) then
>           do putStrLn "invalid input, try again: "
>              getUserInput cb cp
>       else return (fromJust tmpBoard)

Empty determine whether the block is empty

> empty :: Player -> Bool
> empty B = True
> empty _ = False

Determine whetehr board is full

> isFull :: Board -> Bool
> isFull = and . map (all (not . empty)) 

Return rows of board in a list

> getRows :: Board -> [Row]
> getRows = id

Return columns of board in a list

> getCols :: Board -> [Row]
> getCols = transpose . getRows 

Make a move on board

> move :: Int -> Board -> Player -> Maybe Board
> move index board p
>    | index > cols = Nothing
>    | not (empty (head (getRows board) !! index)) = Nothing
>    | otherwise = Just (transpose (xs ++ replacedColumn : ys))
>          where
>            columns = getCols board
>            (xs, onMoveColumn:ys) = splitAt index columns
>            replacedColumn = sink onMoveColumn p 


> sink :: Row -> Player -> Row
> sink column p = xs ++ p:ys
>       where
>           ((_:xs), ys) = partition (\z -> empty z) column

Determine whether the game is over

> gameOver :: Board -> Player -> Result
> gameOver b p | (rowLine b p) || (colLine b p) || (diaLine b p) = Win
>              | (rowLine b (nextPlayer p)) || (colLine b (nextPlayer p)) || (diaLine b (nextPlayer p)) = Fail
>              | isFull(b) = Draw
>              | otherwise = Und

Determine whether there are consecutive players' blocks
    
> contains :: Player -> Row -> Bool
> contains p row = isInfixOf (replicate win p) row

Determine whether there are wins of consecutive blocks in a row

> rowLine :: Board -> Player -> Bool
> rowLine board p = or (map (contains p) (getRows board)) 

Determine whether there are wins of consecutive blocks in a column

> colLine :: Board -> Player -> Bool
> colLine board p = or (map (contains p) (getCols board)) 

Determine whether there are wins of consecutive blocks in a diagonal

> diaLine :: Board -> Player -> Bool
> diaLine board p = or (map (contains p) ((getDiagonals board) ++ (getDiagonals (turnN board))))

Take a 90 degree turn

> turnN :: Board -> Board
> turnN = map reverse . transpose 

Getdiagonals as list of lists

> getDiagonals :: Board -> [Row]
> getDiagonals board = findNext [head board] (tail board) where
>   findNext currentDiag notVisited = [h | h:_ <- currentDiag] : case notVisited of
>            [] -> transpose rt
>            (x:xs) -> findNext (x:rt) xs
>            where rt = [t | _:t <- currentDiag]

> data Result = Und | Fail | Draw | Win
>               deriving (Ord, Eq, Show)

> showResult :: Result -> String
> showResult Und = "Undetermine"
> showResult Fail = "Fail"
> showResult Draw = "Draw"
> showResult Win = "Win"

> data GameTree a = Leaf a | Node a [GameTree a]

Generate GameTree from current board

Evaluation Function for current board

> solutions :: Int -> Player -> [[Player]]
> solutions 1 p = [replicate win p]
> solutions n p = rmdups (permutations ((replicate (win-n+1) p) ++ (replicate (n-1) B))) ++ (solutions (n-1) p)
>       where rmdups = map head . group . sort

> credit :: [[Player]]
> credit = solutions win X

> punish :: [[Player]]
> punish = solutions win O

> subs :: Int -> [Player] -> [[Player]]
> subs n s = filter ((n==) . length) (subsequences s) 

> score :: [Row] -> [[Player]] -> Int
> score r ps = sum (map length (map (intersect ps) (map (subs win) r)))

> eval :: Board -> Int
> eval b | r == Win = 1000
>        | r == Fail = -1000
>        | otherwise = (score (getRows b) credit) + (score (getCols b) credit) + (score ((getDiagonals b) ++ (getDiagonals (turnN b))) credit) - (
>                      (score (getRows b) punish) + (score (getCols b) punish) + (score ((getDiagonals b) ++ (getDiagonals (turnN b))) punish)) 
>       where r= gameOver b X  



> generateTree :: Board -> Player -> GameTree Board
> generateTree cb cp = depthDec depth cb cp where
>   depthDec x b p | r == Und = Node b [depthDec (x-1) nb (nextPlayer p) | nb <- pmove b p]
>                  | x == 0 = Leaf b
>                  | otherwise = Leaf b
>                  where r = gameOver b p  

> showGameTree :: GameTree Board -> String
> showGameTree (Leaf bl) = unlines (map (map showPlayer) bl) 
> showGameTree (Node b bc) = unlines (map showRow b) ++ concat [showGameTree bcc | bcc <-bc] 
>   where showRow = map showPlayer

List all possible move

> pmove :: Board -> Player -> [Board]
> pmove cb cp = catMaybes [move n cb cp | n <- [0..cols-1]]   

MiniMax

> minimax :: Int -> Board -> Player -> Int
> minimax 0 b _ = eval b
> minimax n b p | (eval b) == 1000 = 1000
>               | (eval b) == -1000 = -1000
>               | p == O = minimum [minimax (n-1) bc (nextPlayer p) | bc <- pmove b p]
>               | otherwise = maximum [minimax (n-1) bc (nextPlayer p) | bc <- pmove b p]

Get AI to play

> findBest :: [(Int, Board)] -> Board
> findBest [] = error "WTF"
> findBest (x:xs) = maxTail x xs
>   where maxTail currentMax [] = snd currentMax
>         maxTail (s, b) (t:ts) | s < (fst t) = maxTail t ts
>                                   | otherwise = maxTail (s, b) ts

> getRobotInput :: Board -> Player -> Board
> getRobotInput cb cp = findBest [(minimax depth board (nextPlayer cp), board) | board <- pmove cb cp]