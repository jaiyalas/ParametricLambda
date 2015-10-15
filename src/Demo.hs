{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternGuards #-}

module Main where

import Lambda.Text.Universal
import ForDemo.BigLambdaEagerReduction

f1 = fun nx $ fun ny $ (vx <> vz)
f2 = (fun nu $ fun nx $ (vu <> vx)) <> (fun nu (vx <> vu))
f3 = fun nx $ fun ny $ fun nz $ vx <> vy <> vz
f4 = vy <> vz <> (vy <> vx)

bl = putStrLn $ "############"

main = do
  bl -- Church Number
  putStrLn $ "Church Number"
  putStrLn $ ("3 = " ++) $ pretty $ betaRed $ (ch3)
  putStrLn $ ("2 = " ++) $ pretty $ betaRed $ (ch2)
  putStrLn $ ("3 + 2 = " ++) $ pretty $ betaRed $ (ch3 ~+~ ch2)
  bl
  return ()
