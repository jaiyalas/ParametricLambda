 {-# LANGUAGE UnicodeSyntax #-}

import Haha.Lambda.NamedVar.Term
import Haha.Lambda.NamedVar.Substitution

f1 = fun "x" $ fun "y" $ (Var "x" <> Var "z")
f2 = (fun "u" (Var "x")) <> (fun "u" (Var "x" <> Var "z"))
f3 = fun "x" $ fun "z" $ Var "u"
f4 = fun "z" $ fun "u" $ Var "u"

main :: IO ()
main = do
  putStrLn $ show f2 f1
