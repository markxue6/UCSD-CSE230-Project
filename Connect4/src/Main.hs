{-# LANGUAGE OverloadedStrings #-}
module Main where
import Prelude
import TwoPlayers (twoPlayers)

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
  $ [ ("2", "Two Player Mode"),
  ("2", "Two Player Mode"),
  ("2", "Two Player Mode")
  ]
    where
      drawK a k = (padRight Max $ padLeft (Pad 1) $ str a)
                        <+> (padLeft Max $ padRight (Pad 1) $ str k)

startGame :: String -> IO ()
startGame s = case s of
  "2" -> twoPlayers
  _ -> twoPlayers

main :: IO ()
main = do
  simpleMain drawCover
  c <- getLine
  startGame c