module GameLogicEasy (Game(..), Player, Board, printPlayer, initGame,
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
  Just b' -> if gameOver b' X then Just b' else Just (nextMove b' (nextPlayer p) (8 * index + 9))

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


type GameNode = (Board, Int, Int)

data GameTree a = Node a [GameTree a]
            deriving (Ord, Eq, Show)

possibleMove :: Board -> Player -> [Board]
possibleMove bd p = catMaybes [move' n bd p | n <- [0..cols-1]]


createGameTree :: Board -> Player -> Int -> GameTree GameNode
createGameTree bd p 1 = Node (bd, 0, depth - 1) [Node (x, evaluateLeaf x O, depth) [] | x <- (possibleMove bd p)]
createGameTree bd p d = Node (bd, 0, depth - d) [Node (x, 0, depth - d + 1) [createGameTree x (nextPlayer p) (d - 1)] | x <- (possibleMove bd p)]

evaluateLeaf :: Board -> Player -> Int
evaluateLeaf bd p
  | gameOver bd p = 1
  | gameOver bd (nextPlayer p) = -1
  | otherwise = 0

findMax :: [GameTree GameNode] -> Int
findMax [] = -1
findMax ((Node (_, a, _) _):xs)
                           | a > (findMax xs) = a
                           | otherwise        = findMax xs

findMin :: [GameTree GameNode] -> Int
findMin [] = 1
findMin ((Node (_, a, _) _):xs)
                           | a < (findMin xs) = a
                           | otherwise        = findMin xs

evaluateNode :: GameTree GameNode -> GameTree GameNode
evaluateNode (Node e [])       = Node e []
evaluateNode (Node (bd, _, d) xs)
                                | even d    = Node (bd, findMax target, d) target
                                | otherwise = Node (bd, findMin target, d) target
                                where target = map evaluateNode xs

selectMove :: Int -> GameTree GameNode -> Board
selectMove _ (Node (bd, _, _) []) = bd
selectMove s (Node (bd, m, a) xs)
                            | m == extractValue (xs !! index) = extractBoard (xs !! index)
                            | otherwise = selectMove (s + 1) (Node (bd, m, a) xs)
                            where (index, _) = randomR (0, (length xs) - 1) (mkStdGen s)

extractValue :: GameTree GameNode -> Int
extractValue (Node (_, v, _) _) = v

extractBoard :: GameTree GameNode -> Board
extractBoard (Node (bd, _, _) _) = bd

nextMove :: Board -> Player -> Int -> Board
nextMove bd p s = selectMove s (evaluateNode (createGameTree bd p depth))