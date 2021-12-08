import Data.List
import Data.Maybe

rows :: Int
rows = 7

cols :: Int
cols = 7

win :: Int
win = 4

initGameTest :: Game
initGameTest = Game { _board = gameBoard
        , _player   = X
        , _done     = False
        }


getWinner :: Game -> String
getWinner game = 
    if (gameOver (_board game) X) then "X wins"
    else if (gameOver (_board game) O) then "O wins"
    else if (isFull (_board game)) then "The board is full"
    else "Game goes on"

reverseList :: [a] -> [a]
reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]

unitTestGame :: [Int] -> String
unitTestGame moves = (show game)  ++ "\n" ++ getWinner game
    where game = foldr moveg initGameTest (reverseList moves)

-- Win by rows
-- >>> putStrLn(unitTestGame [0, 0, 1, 1, 2, 2, 3, 3])
-- Game {_board = [[B,B,B,B,B,B,B],[B,B,B,B,B,B,B],[B,B,B,B,B,B,B],[B,B,B,B,B,B,B],[B,B,B,B,B,B,B],[O,O,O,B,B,B,B],[X,X,X,X,B,B,B]], _player = O, _done = True}
-- X wins
--

-- Win by columns
-- >>> putStrLn(unitTestGame [0, 1, 0, 1, 0, 1, 0, 1])
-- Game {_board = [[B,B,B,B,B,B,B],[B,B,B,B,B,B,B],[B,B,B,B,B,B,B],[X,B,B,B,B,B,B],[X,O,B,B,B,B,B],[X,O,B,B,B,B,B],[X,O,B,B,B,B,B]], _player = O, _done = True}
-- X wins
--

-- The second player wins
-- >>> putStrLn(unitTestGame [4, 0, 1, 0, 1, 0, 1, 0])
-- Game {_board = [[B,B,B,B,B,B,B],[B,B,B,B,B,B,B],[B,B,B,B,B,B,B],[O,B,B,B,B,B,B],[O,X,B,B,B,B,B],[O,X,B,B,B,B,B],[O,X,B,B,X,B,B]], _player = X, _done = True}
-- O wins
--

-- Win by diagonals
-- >>> putStrLn(unitTestGame [0, 1, 1, 0, 2, 2, 2, 3, 3, 3, 3])
-- Game {_board = [[B,B,B,B,B,B,B],[B,B,B,B,B,B,B],[B,B,B,B,B,B,B],[B,B,B,X,B,B,B],[B,B,X,O,B,B,B],[O,X,O,X,B,B,B],[X,O,X,O,B,B,B]], _player = O, _done = True}
-- X wins
--

-- Move out of border
-- >>> putStrLn(unitTestGame [10])
-- Game {_board = [[B,B,B,B,B,B,B],[B,B,B,B,B,B,B],[B,B,B,B,B,B,B],[B,B,B,B,B,B,B],[B,B,B,B,B,B,B],[B,B,B,B,B,B,B],[B,B,B,B,B,B,B]], _player = X, _done = False}
-- Game goes on
--

copyList :: Int -> [Int] -> [Int]
copyList 0 _ = []
copyList 1 xs = xs
copyList n xs = (copyList (n-1) xs) ++ xs


-- The board is filled but no winner
filledMoves :: [Int]
filledMoves = ((copyList 3 [0, 1, 1, 0]) ++ [0, 1]) ++ ((copyList 3 [3, 2, 2, 3]) ++ [3, 2]) ++ ((copyList 3 [4, 5, 5, 4]) ++ [4, 5]) ++ (replicate 7 6)
-- >>> putStrLn(unitTestGame filledMoves)
-- Game {_board = [[X,O,O,X,X,O,X],[O,X,X,O,O,X,O],[X,O,O,X,X,O,X],[O,X,X,O,O,X,O],[X,O,O,X,X,O,X],[O,X,X,O,O,X,O],[X,O,O,X,X,O,X]], _player = O, _done = True}
-- The board is full
--



data Player = O | B | X
              deriving (Ord, Eq, Show)

type Board = [Row]
type Row = [Player]

data Game = Game
  { _board   :: Board
  , _player :: Player
  , _done    :: Bool
  } deriving (Eq, Show)

gameBoard :: Board
gameBoard = replicate rows (replicate cols B)

nextPlayer :: Player -> Player
nextPlayer X = O
nextPlayer O = X
nextPlayer _ = B



empty :: Player -> Bool
empty B = True
empty _ = False

isFull :: Board -> Bool
isFull = and . map (all (not . empty))

getRows :: Board -> [Row]
getRows = id

getCols :: Board -> [Row]
getCols = transpose . getRows 


move :: Int -> Board -> Player -> Maybe Board
move index board p
   | index > cols = Nothing
   | not (empty (head (getRows board) !! index)) = Nothing
   | otherwise = Just (transpose (xs ++ replacedColumn : ys))
         where
           columns = getCols board
           (xs, onMoveColumn:ys) = splitAt index columns
           replacedColumn = sink onMoveColumn p 


sink :: Row -> Player -> Row
sink column p = xs ++ p:ys
      where
          ((_:xs), ys) = partition (\z -> empty z) column

gameOver :: Board -> Player -> Bool
gameOver b p | (rowLine b p) || (colLine b p) || (diaLine b p) = True
             | otherwise = False

    
contains :: Player -> Row -> Bool
contains p row = isInfixOf (replicate win p) row

rowLine :: Board -> Player -> Bool
rowLine board p = or (map (contains p) (getRows board)) 

colLine :: Board -> Player -> Bool
colLine board p = or (map (contains p) (getCols board)) 

diaLine :: Board -> Player -> Bool
diaLine board p = or (map (contains p) ((getDiagonals board) ++ (getDiagonals (turnN board))))

turnN :: Board -> Board
turnN = map reverse . transpose 

getDiagonals :: Board -> [Row]
getDiagonals board = findNext [head board] (tail board) where
  findNext currentDiag notVisited = [h | h:_ <- currentDiag] : case notVisited of
           [] -> transpose rt
           (x:xs) -> findNext (x:rt) xs
           where rt = [t | _:t <- currentDiag]


initGame :: IO Game
initGame = do
  pure $ 
      Game { _board = gameBoard
        , _player   = X
        , _done     = False
        }

printPlayer :: Player -> String
printPlayer t = case t of
 O -> "O"
 X -> "X"
 _ -> " "

isGameOver :: Game -> Bool
isGameOver g = gameOver b p
  where b = _board g
        p = _player g

moveg :: Int -> Game -> Game
moveg dir (Game {_board = a, _player = b, _done = True}) = Game {_board = a, _player = b, _done = True}
moveg dir g =
  Game {  _board = newBoard
        , _player = newP
        , _done = newDone
        }
  where 
    newBoard = case (_done g) of
      True -> _board g
      False -> case move dir (_board g) (_player g) of
        Nothing -> _board g
        Just bd -> bd
    newP = case move dir (_board g) (_player g) of
      Nothing  -> _player g
      Just bd -> (nextPlayer (_player g))
    newG = Game {_board = newBoard, _player = (_player g), _done = False}
    newDone = (isGameOver newG) || (isFull (_board newG))
