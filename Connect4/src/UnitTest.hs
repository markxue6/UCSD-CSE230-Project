module UnitTest where
import GameLogicTwo
import TwoPlayers


reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]

moves1 :: [Int]
moves1 = reverseList ([0, 0, 1, 1, 2, 2, 3, 3])

finalGame1 :: Game
finalGame1 = foldr moveg initGameForTest moves1

test1 :: Bool
test1 = isGameOver(finalGame1)

-- >>> finalGame1
-- Game {_board = [[B,B,B,B,B,B,B],[B,B,B,B,B,B,B],[B,B,B,B,B,B,B],[B,B,B,B,B,B,B],[B,B,B,B,B,B,B],[O,O,O,B,B,B,B],[X,X,X,X,B,B,B]], _player = O, _done = True}
--

-- >>> isGameOver(finalGame1)
-- False
--

-- >>> gameOver(_board finalGame1) O
-- False