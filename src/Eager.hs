{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}

module Main where

import Haha.Lambda.NamedVar.Term
import Haha.Lambda.NamedVar.NameSugar
--
import Haha.Lambda.NamedVar.BigLambdaEagerReduction
-- import Haha.Lambda.NamedVar.Reduction
--
import Control.Lens       ((^.))
import Brick.Types        (Widget)
import Brick.Widgets.Core (str)
import Brick.Widgets.List (listSelectedL)
import Haha.MyBrick
--

f1 = fun nx $ fun ny $ (vx <> vz)
f2 = (fun nu $ fun nx $ (vu <> vx)) <> (fun nu (vx <> vu))
f3 = fun nx $ fun ny $ fun nz $ vx <> vy <> vz
f4 = vy <> vz <> (vy <> vx)

--

terms = [myK <> (myI <> vy)
        ,myK <> (myI <> vx)
        ,myK <> (myK <> (myI <> vy))
        ,f1
        ,f2
        -- ,(fun nx $ vx <> vx) <> (fun nx $ vx <> vx) -- !!!!!!!!
        ]

{- ########################################## -}

instance MenuShow Term where
  menuShow t = cutLength 90 $ pretty t
--
instance TriPanel Term where
  mainF   ts l | Nothing <- l ^. listSelectedL = str "-"
  mainF   ts l | Just i  <- l ^. listSelectedL =
    let res = betaRed $ ts !! i
    in str $ cutLength 100 $ (pretty res) ++ "\n\n" ++ (show res)
  footF   ts l | Nothing <- l ^. listSelectedL = str "-"
  footF   ts l | Just i  <- l ^. listSelectedL = case (ts !! i) of
    (App (Abs a t) q) -> str $
      -- (prettyList $ fv $ (App (Abs a t) q))++
      "lambda <"++a++">: "++
      (prettyList $ binder a t)++
      " || fv in RHS: "++
      (prettyList $ fv $ q)++" "
    t         -> str $ prettyList $ fv $ t

--

prettyList :: [Term] -> String
prettyList []     = "[]"
prettyList [x]    = "["++(pretty x)++"]"
prettyList (x:xs) = "[" ++ (pretty x) ++ ", " ++ hs
  where hs = tail $ prettyList xs

--

main :: IO ()
main = initMain terms

-- ## EXTRA TOOLS

cutLength :: Int -> String -> String
cutLength n xs | n >= length xs = xs
cutLength n xs | n <  length xs = (take n xs)
    ++ "\n"
    ++ cutLength n (drop n xs)
