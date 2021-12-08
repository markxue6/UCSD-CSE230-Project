module GameLogicOne (Game(..), Player, Board, printPlayer, initGame,
              nextPlayer,  move, gameOver, isFull)
        where
import Data.List
import Data.Maybe
import System.Random

rows :: Int
rows = 7

cols :: Int
cols = 7

win :: Int
win = 4

depth :: Int
depth = 3

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


move' :: Int -> Board -> Player -> Maybe Board
move' index board p
   | index > cols = Nothing
   | not (empty (head (getRows board) !! index)) = Nothing
   | otherwise = Just (transpose (xs ++ replacedColumn : ys))
         where
           columns = getCols board
           (xs, onMoveColumn:ys) = splitAt index columns
           replacedColumn = sink onMoveColumn p 

move :: Int -> Board -> Player -> Maybe Board
move index board p = case move' index board p of
  Nothing -> Nothing 
  Just b' -> move' i b' (nextPlayer p)
    where
      i = mod (index * 9 + 3) 7 + 1


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