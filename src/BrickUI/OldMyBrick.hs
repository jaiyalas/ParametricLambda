{-# LANGUAGE OverloadedStrings #-}

module BrickUI.OldMyBrick
  (initMain
  , TriPanel (..)
  , MenuShow (..)
  ) where

import Control.Monad (void)
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
  -- , hLimit
  , hBox
  , vBox
  , withAttr
  -- , padRight
  -- , padLeft
  -- , padTop
  -- , padBottom
  , padTopBottom
  , padLeftRight
  )
import Brick.Util (fg, on)

class Show a => MenuShow a where
  menuShow :: a -> String

class Show a => TriPanel a where
  headerF :: [a] -> L.List a -> Widget
  mainF   :: [a] -> L.List a -> Widget
  footF   :: [a] -> L.List a -> Widget
  headerT :: [a] -> L.List a -> Widget
  mainT   :: [a] -> L.List a -> Widget
  footT   :: [a] -> L.List a -> Widget

drawUI :: (Eq a, TriPanel a, MenuShow a) => [a] -> L.List a -> [Widget]
drawUI ts l = [ui]
    where
        menu = vLimit 3 $ L.renderList l (listDrawElement ts)
        ui = vBox [ padTopBottom 0 $ menu
                    , B.hBorder
                    , vBox [
                          padTopBottom 0 $ vLimit 4 $ hBox [
                              padLeftRight 0 $ headerT ts l
                              , padLeftRight 1 $ C.hCenter $ C.vCenter $ headerF ts l]
                        , B.hBorder
                        , padTopBottom 0 $ mainT ts l
                        , C.vCenter $ C.hCenter $ mainF ts l
                        , B.hBorder
                        , padTopBottom 0 $ vLimit 1 $ hBox [
                              padLeftRight 0 $ footT ts l
                              , padLeftRight 1 $ footF ts l]
                        ]
                    ]

-- event handler for keyboard events
appEvent :: L.List a -> V.Event -> T.EventM (T.Next (L.List a))
appEvent l e =
    case e of
        V.EvKey V.KEsc [] -> M.halt l
        ev -> M.continue =<< T.handleEvent ev l

-- how to draw list
listDrawElement :: (Eq a, MenuShow a) => [a] -> Bool -> a -> Widget
listDrawElement ts sel i =
    let selStr s = if sel
                   then withAttr customAttr (str $ s)
                   else str s
    in     str (show $ (+1) $ Prelude.head $ elemIndices i ts)
       <+> str "/"
       <+> str (show $ Prelude.length ts)
       <+> str ") " <+> (selStr $ menuShow i)
    -- in C.hCenter $ str "> " <+> (selStr $ menuShow i)

customAttr :: A.AttrName
customAttr = L.listSelectedAttr <> "custom"

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
    [ (L.listAttr,            V.white `on` V.blue)
    , (L.listSelectedAttr,    V.red `on` V.white)
    , (customAttr,            fg V.black)
    ]

initialState :: [a] -> L.List a
initialState terms = L.list (T.Name "list") (Data.Vector.fromList terms) 1

theApp :: (Eq a, TriPanel a, MenuShow a) => [a] -> M.App (L.List a) V.Event
theApp ts =
    M.App { M.appDraw = drawUI ts
          , M.appChooseCursor = M.showFirstCursor
          , M.appHandleEvent = appEvent
          , M.appStartEvent = return
          , M.appAttrMap = const theMap
          , M.appLiftVtyEvent = id
          }

initMain :: (Eq a, TriPanel a, MenuShow a) => [a] -> IO ()
initMain ts = void $ M.defaultMain (theApp ts) (initialState ts)
