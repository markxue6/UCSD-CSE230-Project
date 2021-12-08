{-# LANGUAGE OverloadedStrings #-}
module Main where
import Prelude
import TwoPlayers (twoPlayers)
import TwoPlayersLarge (twoPlayersLarge)
import OnePlayerEasy (onePlayerEasy)
import OnePlayerHard (onePlayerHard)

import Brick
  ( App(..), AttrMap, BrickEvent(..), EventM, Next, Widget
  , customMain, neverShowCursor, attrName, simpleMain
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

drawCover :: Widget ()
drawCover = withBorderStyle BS.unicodeBold
  $ C.hCenter
  $ vLimit 400
  $ hLimit 80
  $ B.borderWithLabel (str "Welcome To the Connect 4 Game")
  $ vBox $ map (uncurry drawK)
  $ [ ("1", "Two Player Mode"),
  ("2", "9*9 Board Two Player Mode"),
  ("3", "Play With AI (Easy)"),
  ("4", "Play With AI (Hard)")
  ]
    where
      drawK a k = (padRight Max $ padLeft (Pad 1) $ str a)
                        <+> (padLeft Max $ padRight (Pad 1) $ str k)

startGame :: String -> IO ()
startGame s = case s of
  "1" -> twoPlayers
  "2" -> twoPlayersLarge
  "3" -> onePlayerEasy
  "4" -> onePlayerHard
  _   -> error "Invalid input"

main :: IO ()
main = do
  simpleMain drawCover
  c <- getLine
  startGame c