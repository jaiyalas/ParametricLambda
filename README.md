# ParametricLambda

[![Build Status](https://api.travis-ci.org/jaiyalas/ParametricLambda.png?branch=master)](http://travis-ci.org/jaiyalas/ParametricLambda)
[![MIT](http://b.repl.ca/v1/license-MIT-blue.png)](https://en.wikipedia.org/wiki/MIT_License)
[![Haskell](http://b.repl.ca/v1/language-haskell-orange.png)](http://haskell.org)

Build Parametric Lambda Calculus.

## Todo

+ 想辦法解決 InputSet (Γ/γ/Λ) 的問題
  + 也許 functional dependency 會是個好辦法?
+ 擴充 Term 成可以用 zipper
  + 方案 A: ` data TermWithHead = ... `
  + 方案 B: 用 one-hole context 訂個 `TermZipper`
+ Build my own lambda evaluator.
+ Deriving a VM and a compiler.
+ Speedup
