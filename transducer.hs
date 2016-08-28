module Transducer where

import           Definitions
import           Text.Printf (printf)
import           Utils       (getFreshInt, getFreshString)

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

generateCode :: Transducer -> EvalState String
generateCode tr = do
    body <- ($ "") <$> (code tr) tr
    n <- getFreshInt
    let WPort (qr, nr) = outputWire tr
    return $ printf "\
\#include <stdio.h>\n\
\ \n\
\int main(void) {\n\
\    int acc = 0;\n\
\    static int mem[%d];\n\
\    goto %s;\n\n\
\%s\n\
\%s:\n\
\    %s\n\
\}\n" n qr body nr "printf(\"%d\\n\", acc);"
