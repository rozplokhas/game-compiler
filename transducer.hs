module Transducer where

import Definitions

wireByType :: Type -> EvalState Wire
wireByType N = do
    s <- getFreshString
    return $ WPort ('q' : s, 'n' : s)
wireByType (Times t1 t2) = do
    w1 <- wireByType t1
    w2 <- wireByType t2
    return (WTimes w1 w2)
wireByType (Arrow t1 t2) = do
    w1 <- wireByType t1
    w2 <- wireByType t2
    return (WArrow w1 w2)

freshWire :: Wire -> EvalState Wire
freshWire (WPort _) = do
    s <- getFreshString
    return $ WPort ('q' : s, 'n' : s)
freshWire (WTimes u1 u2) = do
    w1 <- freshWire u1
    w2 <- freshWire u2
    return (WTimes w1 w2)
freshWire (WArrow u1 u2) = do
    w1 <- freshWire u1
    w2 <- freshWire u2
    return (WArrow w1 w2)

createTransducer :: TransducerDescr -> EvalState Transducer
createTransducer (printer, Signature ts t) = do
    outw <- wireByType t
    inw <- mapM wireByType ts
    return $ Transducer inw outw printer

generateCode :: Transducer -> String
generateCode tr = (code tr) tr ""
