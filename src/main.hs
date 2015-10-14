{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import Haha.Lambda.NamedVar.Lambda
import Haha.MyBrick

--

f1 = fun nx $ fun ny $ (vx <> vz)
f2 = (fun nu $ fun nx $ (vu <> vx)) <> (fun nu (vx <> vu))
f3 = fun nx $ fun ny $ fun nz $ vx <> vy <> vz
f4 = vy <> vz <> (vy <> vx)

--
iss :: [InputSet]
iss = [IS_Lambda, IS_Gamma, IS_gamma]
terms :: [Term]
terms = [ myK <> (myI <> vy)
        , myK <> (myI <> vx)
        , myK <> (myK <> (myI <> vy))
        , f1
        , f2
        , myI <> (nx ~> vz <> vy)
        , myI <> (nx ~> vx <> vx)
        -- ,(fun nx $ vx <> vx) <> (fun nx $ vx <> vx) -- !!!!!!!!
        ]


{- ########################################## -}


instance Display Term InputSet where
  mainWidget (Var a)   is = "..."
  mainWidget (Abs a t) is = "..."
  mainWidget (App p q) is = "..."
  {-| ############# |-}
  footWidget (Var a)   is = ""
  footWidget (Abs a t) is = ""
  footWidget (App p q) is | Abs a t <- p
                          , inInputSet is q
    = (pretty q) ++ " is in " ++ (show is)
  footWidget (App p q) is | Abs a t <- p
                          , not $ inInputSet is q
    = "NOT in input set: "++(show is)
  footWidget (App p q) is | otherwise
    = prettyList $ fv $ (App p q)

{-
"lambda <"++a++">: "++
(prettyList $ binder a t)++
" || fv in RHS: "++
(prettyList $ fv q)
-}

instance ShowMenu Term where
  showMenu = pretty

instance ShowMenu InputSet where
  showMenu = show

--

prettyList :: [Term] -> String
prettyList []     = "[]"
prettyList [x]    = "["++(pretty x)++"]"
prettyList (x:xs) = "[" ++ (pretty x) ++ ", " ++ hs
  where hs = tail $ prettyList xs

--

main :: IO ()
main = initMain terms iss

-- ## EXTRA TOOLS

cutLength :: Int -> String -> String
cutLength n xs | n >= length xs = xs
cutLength n xs | n <  length xs = (take n xs)
    ++ "\n"
    ++ cutLength n (drop n xs)
