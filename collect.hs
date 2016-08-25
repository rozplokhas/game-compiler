module Collect (buildTransducer) where

import           ConstTransducers
import           Data.Map         as M (Map, singleton)
import           Definitions
import           Rules
import           Transducer

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
    application whileT caT



testWhile = do
    o1 <- createTransducer (numberDescr 0)
    o2 <- createTransducer (numberDescr 0)
    o <- prod o1 o2
    wh <- createTransducer whileDescr
    ap <- application wh o
    generateCode ap

testVar = do
    ten <- createTransducer (numberDescr 10)
    v <- createTransducer newVarDescr
    r <- createTransducer derefDescr
    a <- application r v
    p <- prod a ten
    s <- createTransducer seqDescr
    r <- application s p
    generateCode r
