module Collect (buildTransducer) where

import Data.Map as M (Map, singleton)
import Definitions
import Rules
import Transducer
import ConstTransducers

buildTransducer :: AST -> EvalState Transducer

buildTransducer (IConst n) = createTransducer (numberDescr n)

buildTransducer (BinOp op x y) = do
    xt <- buildTransducer x
    yt <- buildTransducer y
    xy <- prod xt yt
    p <- createTransducer (binOpDescr op)
    application p xy

buildTransducer (IfThenElse c t f) = do
    cT <- buildTransducer c
    tT <- buildTransducer t
    fT <- buildTransducer f
    ctT <- prod cT tT
    ctfT <- prod ctT fT
    ifT <- createTransducer ifDescr
    application ifT ctfT

buildTransducer (While c a) = do
    cT <- buildTransducer c
    aT <- buildTransducer a
    caT <- prod cT aT
    whileT <- createTransducer whileDescr
    application whileT aT
