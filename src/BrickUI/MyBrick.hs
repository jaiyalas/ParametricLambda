{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module BrickUI.MyBrick
  (initMain
  , Display (..)
  , ShowMenu (..)
  ) where

import Control.Monad (void)
import Control.Lens (_1,_2,(%~),(&),makeLenses,(^.))
import Data.Monoid ((<>))
import Data.List (elemIndices)
import qualified Data.Vector (fromList)

import qualified Graphics.Vty as V
import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.List as L
import qualified Brick.Widgets.Center as C
import qualified Brick.AttrMap as A

import Brick.Types (Widget)
import Brick.Widgets.Core
  ( (<+>)
  , str
  , vLimit
  , hLimit
  , hBox
  , vBox
  , withAttr
  -- , padRight
  -- , padLeft
  -- , padTop
  -- , padBottom
  , padTopBottom
  , padLeftRight
  , viewport
  , visible
  )
import Brick.Util (fg, on)

data Select = Select {_termIdx :: Int, _isIdx :: Int}
            deriving Show

makeLenses ''Select

termdAttr :: A.AttrName
termdAttr = "term"
isetAttr :: A.AttrName
isetAttr = "iset"
menuAttr :: A.AttrName
menuAttr = "menu"
theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
    [ (termdAttr,  V.black `on` V.yellow)
    , (isetAttr, V.white `on` V.red)
    , (menuAttr,  V.white `on` V.blue)
    ]

class (Show a, Show b) => Display a b where
  mainWidget :: a -> b -> String
  footWidget :: a -> b -> String

class Show a => ShowMenu a where
  showMenu :: a -> String

drawUI :: (ShowMenu a, ShowMenu b, Display a b) => [a] -> [b] -> Select -> [Widget]
drawUI ts iss st =
    let ui = -- C.center $ hLimit 60 $ vLimit 30 $
             vBox [ padTopBottom 0 $ withAttr menuAttr $ vLimit 3 $
                      hBox [ menuA
                           , B.vBorder
                           , hLimit 5 $ menuB ]
                  , B.hBorder -- ################################## --
                  , C.vCenter $ C.hCenter $ str $
                      mainWidget (ts !! (st^.termIdx - 1)) (iss !! (st^.isIdx - 1))
                  , B.hBorder -- ################################## --
                  , padTopBottom 0 $ vLimit 1 $
                      hBox [ padLeftRight 0 $ str $ "[Message]"
                           , padLeftRight 1 $ str $ footWidget (ts !! (st^.termIdx - 1))
                                                               (iss !! (st^.isIdx - 1))
                           ]
                  ]
        menuA = viewport "Terms" T.Vertical $
                  vBox $ do
                      i <- [1..length ts]
                      let mkItem = if i == st^.termIdx
                                   then withAttr termdAttr . visible
                                   else id
                          menuStr = " " <> showMenu (ts !! (i - 1)) <> " "
                      return $ mkItem $ str $ show i <> "." <> menuStr
        menuB = viewport "InputSets" T.Vertical $
                  vBox $ do
                      i <- [1..length iss]
                      let mkItem = if i == st^.isIdx
                                   then withAttr isetAttr . visible
                                   else id
                          menuStr = " " <> showMenu (iss !! (i - 1)) <> " "
                      return $ C.hCenter $ mkItem $ str $ menuStr
    in [ui]

appEvent :: Int -> Int -> Select -> V.Event -> T.EventM (T.Next Select)
appEvent s1 s2 st (V.EvKey V.KDown  []) = M.continue $ st & termIdx %~ min s1 . (+ 1)
appEvent s1 s2 st (V.EvKey V.KUp    []) = M.continue $ st & termIdx %~ max 1 . subtract 1
appEvent s1 s2 st (V.EvKey V.KRight []) = M.continue $ st & isIdx %~ min s2 . (+ 1)
appEvent s1 s2 st (V.EvKey V.KLeft  []) = M.continue $ st & isIdx %~ max 1 . subtract 1
appEvent s1 s2 st (V.EvKey V.KEsc   []) = M.halt st
appEvent s1 s2 st _ = M.continue st

theApp :: (ShowMenu a, ShowMenu b, Display a b) => [a] -> [b] -> M.App Select V.Event
theApp ts iss = M.App { M.appDraw         = drawUI ts iss
                      , M.appAttrMap      = const theMap
                      , M.appHandleEvent  = appEvent (length ts) (length iss)
                      , M.appStartEvent   = return
                      , M.appLiftVtyEvent = id
                      , M.appChooseCursor = M.neverShowCursor
                      }

initialState :: Select
initialState = Select 1 1

initMain :: (ShowMenu a, ShowMenu b, Display a b) => [a] -> [b] -> IO ()
initMain ts iss = void $ M.defaultMain (theApp ts iss) initialState
