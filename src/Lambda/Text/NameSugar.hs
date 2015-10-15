module Lambda.Text.NameSugar where

import Lambda.Text.Term

ns, nt, nu, nv, nw, nx, ny, nz :: VName
nf = "f"
ng = "g"
nh = "h"
ns = "s"
nt = "t"
nu = "u"
nv = "v"
nw = "w"
nx = "x"
ny = "y"
nz = "z"

vs, vt, vu, vv, vw, vx, vy, vz :: Term
vf = Var nf
vg = Var ng
vh = Var nh
vs = Var ns
vt = Var nt
vu = Var nu
vv = Var nv
vw = Var nw
vx = Var nx
vy = Var ny
vz = Var nz

myI, myK, myW, myC, myB, myS :: Term
myI = fun nx vx
myK = fun nx $ fun ny $ vx
myW = fun nf $ fun nx $ (vf <> vx) <> vx
myC = fun nf $ fun nx $ fun ny $ (vf <> vy) <> vx
myB = fun nf $ fun ng $ fun nx $ vf <> (vg <> vx)
myS = fun nf $ fun ng $ fun nx $ (vf <> vx) <> (vg <> vx)
