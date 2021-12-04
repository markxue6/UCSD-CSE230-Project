{-# LANGUAGE OverloadedStrings #-}
module TwoPlayers (twoPlayers, Tick, MyT, drawUI, theMap, moveg, initGame) where
import GameLogic
import Data.Maybe
import Data.List
import Prelude
import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay, forkIO)
import Data.Maybe (fromMaybe)
import Brick
  ( App(..), AttrMap, BrickEvent(..), EventM, Next, Widget
  , customMain, neverShowCursor, attrName
  , continue, halt
  , hLimit, vLimit, vBox, hBox
  , padRight, padLeft, padTop, padAll, Padding(..)
  , withBorderStyle
  , str
  , attrMap, withAttr, emptyWidget, AttrName, on, fg
  , (<+>)
  )
import Brick.BChan (newBChan, writeBChan)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Brick.Util as U
import qualified Graphics.Vty as V
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import Linear.V2 (V2(..))
-- marks passing of time
data Tick = Tick

type MyT = ()

-- color attributes:

gameOverAttr, blueBg, brblBg, cyanBg, bcyanBg, yellowBg, byellowBg, greenBg, bgreenBg,  whiteBg  :: AttrName
gameOverAttr = "gameOver"
blueBg = attrName "blueBg"
brblBg = attrName "brblBg"
cyanBg = attrName "cyanBg"
bcyanBg = attrName "bcyanBg"
magBg = attrName "magBg"
bmagBg = attrName "bmagBg"
yellowBg = attrName "yellowBg"
byellowBg = attrName "byellowBg"
greenBg = attrName "greenBg"
bgreenBg = attrName "bgreenBg"
whiteBg = attrName "whiteBg"

theMap :: AttrMap
theMap = attrMap V.defAttr
  [
  (gameOverAttr, fg V.red `V.withStyle` V.bold),
  (blueBg, U.fg V.blue),
  (brblBg, U.fg V.brightBlue),
  (cyanBg, U.fg V.cyan),
  (bcyanBg, U.fg V.brightCyan),
  (yellowBg, U.fg V.yellow),
  (byellowBg, U.fg V.brightYellow),
  (magBg, U.fg V.magenta),
  (bmagBg, U.fg V.brightMagenta),
  (greenBg, U.fg V.green),
  (bgreenBg, U.fg V.brightGreen),
  (whiteBg, U.bg V.white)
  ]

-- define App
app :: App Game Tick MyT
app = App { appDraw = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return
          , appAttrMap = const theMap
          }

twoPlayers :: IO ()
twoPlayers = do
  chan <- newBChan 10
  forkIO $ forever $ do
    writeBChan chan Tick
    threadDelay 100000 
  g <- initGame
  void $ customMain (V.mkVty V.defaultConfig) (Just chan) app g

isGameOver :: Game -> Bool
isGameOver g = gameOver b p
  where b = _board g
        p = _player g

step :: Game -> Game
step g =
  if isGameOver g then Game {_board = _board g, _player = _player g, _done = True}
  else g

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


handleEvent :: Game -> BrickEvent MyT Tick -> EventM MyT (Next Game)
handleEvent g (AppEvent Tick)                       = continue $ step g
handleEvent g (VtyEvent (V.EvKey (V.KChar '1') [])) = continue $ moveg 0 g
handleEvent g (VtyEvent (V.EvKey (V.KChar '2') [])) = continue $ moveg 1 g
handleEvent g (VtyEvent (V.EvKey (V.KChar '3') [])) = continue $ moveg 2 g
handleEvent g (VtyEvent (V.EvKey (V.KChar '4') [])) = continue $ moveg 3 g
handleEvent g (VtyEvent (V.EvKey (V.KChar '5') [])) = continue $ moveg 4 g
handleEvent g (VtyEvent (V.EvKey (V.KChar '6') [])) = continue $ moveg 5 g
handleEvent g (VtyEvent (V.EvKey (V.KChar '7') [])) = continue $ moveg 6 g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'r') [])) = liftIO (initGame) >>= continue
handleEvent g (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt g
handleEvent g (VtyEvent (V.EvKey V.KEsc []))        = halt g
handleEvent g _                                     = continue g



-- Drawing
drawUI :: Game -> [Widget MyT]
drawUI g = [C.center $ padRight (Pad 1) (drawGameStatus g) <+> (drawBoard g) <+> padLeft (Pad 1) drawInfo]

drawInfo :: Widget MyT
drawInfo = withBorderStyle BS.unicodeBold
  $ hLimit 40
  $ B.borderWithLabel (str "Commands")
  $ vBox $ map (uncurry drawKey)
  $ [ ("nth Column", "number n")
    , ("Restart", "r")
    , ("Quit", "q or esc")
    ]
  where
    drawKey act key = (padRight Max $ padLeft (Pad 1) $ str act)
                      <+> (padLeft Max $ padRight (Pad 1) $ str key)

drawGameStatus :: Game -> Widget MyT
drawGameStatus g =
  if (not (_done g))
    then hLimit 11 $ withAttr gameOverAttr $ C.hCenter $ str ((printPlayer (_player g)) ++ "'s  move")
    else case isFull (_board g) of
      True -> hLimit 11 $ withAttr gameOverAttr $ C.hCenter $ str "Board is Full"
      _    -> hLimit 11 $ withAttr gameOverAttr $ C.hCenter $ str ((printPlayer (nextPlayer (_player g))) ++ " win")


colorTile val = case val of
  "O" -> withAttr blueBg $ str val
  "X" -> withAttr bgreenBg $ str val
  _ -> str val
  
drawBoard :: Game -> Widget MyT
drawBoard g = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (withAttr magBg $ str "Connect4")
  $ vBox rows
  where
    rows = [hBox $ tilesInRow r | r <- (_board g)]
    tilesInRow row = [hLimit 6 $ withBorderStyle BS.unicodeBold $ B.border $ C.hCenter $ padAll 0 $ colorTile $ printPlayer pl | pl <- row]