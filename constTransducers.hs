module ConstTransducers where

import qualified Data.Map    as M ((!))
import           Definitions
import           Text.Printf (printf)

numberCode :: Int -> CodePrinter
numberCode d Transducer{outputWire = WPort (q, n)} =
    return $ prepend $ printf "\
\%s:\n\
\    acc = %d;\n\
\    goto %s;\n" q d n

binOpCode :: String -> CodePrinter
binOpCode op Transducer{outputWire = WArrow (WTimes (WPort (qx, nx)) (WPort (qy, ny))) (WPort (qr, nr))} = do
    i <- getFreshInt
    return $ prepend $ printf "\
\%s:\n\
\    goto %s;\n\
\%s:\n\
\    mem[%d] = acc;\n\
\    goto %s;\n\
\%s:\n\
\    acc = mem[%d] %s acc;\n\
\    goto %s;\n" qr qx nx i qy ny i op nr

seqCode :: CodePrinter
seqCode Transducer{outputWire = WArrow (WTimes (WPort (qx, nx)) (WPort (qy, ny))) (WPort (qr, nr))} =
    return $ prepend $ printf "\
\%s:\n\
\    goto %s;\n\
\%s:\n\
\    goto %s;\n\
\%s:\n\
\    goto %s;\n" qr qx nx qy ny nr

ifCode :: CodePrinter
ifCode Transducer{outputWire = WArrow (WTimes (WTimes (WPort (qc, nc)) (WPort (qt, nt))) (WPort (qf, nf))) (WPort (qr, nr))} =
    return $ prepend $ printf "\
\%s:\n\
\    goto %s;\n\
\%s:\n\
\    if (acc)\n\
\        goto %s;\n\
\    else\n\
\        goto %s;\n\
\%s:\n\
\    goto %s;\n\
\%s:\n\
\    goto %s;\n" qr qc nc qt qf nt nr nf nr

whileCode :: CodePrinter
whileCode Transducer{outputWire = WArrow (WTimes (WPort (qc, nc)) (WPort (qa, na))) (WPort (qr, nr))} =
    return $ prepend $ printf "\
\%s:\n\
\    goto %s;\n\
\%s:\n\
\    if (acc)\n\
\        goto %s;\n\
\    else\n\
\        goto %s;\n\
\%s:\n\
\    goto %s;\n" qr qc nc qa nr na qc

wireToList :: Bool -> Wire -> [((Label, Label), Bool)]
wireToList f (WPort p) = [(p, f)]
wireToList f (WTimes w1 w2) = wireToList f w1 ++ wireToList f w2
wireToList f (WArrow w1 w2) = wireToList (not f) w1 ++ wireToList f w2

connect :: ((Label, Label), Bool) -> ((Label, Label), Bool) -> String
connect ((q, n), False) ((q', n'), _) = printf "\
\%s:\n\
\    goto %s;\n\
\%s:\n\
\    goto %s;\n" q q' n' n

connect ((q, n), True) ((q', n'), _) = printf "\
\%s:\n\
\    goto %s;\n\
\%s:\n\
\    goto %s;\n" q' q n n'

connectWires :: Wire -> Wire -> String
connectWires w1 w2 = concat $ zipWith connect (wireToList False w1) (wireToList False w2)

variableCode :: String -> CodePrinter
variableCode s Transducer{inputWires = inpWs, outputWire = outW} =
    let inpW = inpWs M.! s
    in return $ prepend $ connectWires outW inpW

newVarCode :: CodePrinter
newVarCode Transducer{outputWire = WTimes (WPort (qr, nr)) (WArrow (WPort (qv, nv)) (WPort (qw, nw)))} = do
    i <- getFreshInt
    return $ prepend $ printf "\
\%s:\n\
\    acc = mem[%d];\n\
\    goto %s;\n\
\%s:\n\
\    goto %s;\n\
\%s:\n\
\    mem[%d] = acc;\n\
\    goto %s;\n" qr i nr qw qv nv i nw

derefCode :: CodePrinter
derefCode Transducer{outputWire = WArrow (WTimes (WPort (qr, nr)) (WArrow (WPort (qv, nv)) (WPort (qw, nw)))) (WPort (qa, na))} =
    return $ prepend $ printf "\
\%s:\n\
\%s:\n\
\// dead-end\n\
\%s:\n\
\    goto %s;\n\
\%s:\n\
\    goto %s;\n" nw qv qa qr nr na

assignCode :: CodePrinter
assignCode Transducer{outputWire = WArrow (WTimes (WTimes (WPort (qr, nr)) (WArrow (WPort (qx, nx)) (WPort (qw, nw)))) (WPort (qv, nv))) (WPort (qa, na))} = do
    i <- getFreshInt
    return $ prepend $ printf "\
\%s:\n\
\// dead-end\n\
\%s:\n\
\    goto %s;\n\
\%s:\n\
\    mem[%d] = acc;\n\
\    goto %s;\n\
\%s:\n\
\    acc = mem[%d];\n\
\    goto %s;\n\
\%s:\n\
\    goto %s;\n" nr qa qv nv i qw qx i nx nw na


numberDescr :: Int -> TransducerDescr
numberDescr n = (numberCode n, closedSig N)

binOpDescr :: String -> TransducerDescr
binOpDescr s = (binOpCode s, closedSig (Arrow (Times N N) N))

seqDescr :: TransducerDescr
seqDescr = (seqCode, closedSig (Arrow (Times N N) N))

ifDescr :: TransducerDescr
ifDescr = (ifCode, closedSig (Arrow (Times (Times N N) N) N))

whileDescr :: TransducerDescr
whileDescr = (whileCode, closedSig (Arrow (Times N N) N))

newVarDescr :: TransducerDescr
newVarDescr = (newVarCode, closedSig (Times N (Arrow N N)))

derefDescr :: TransducerDescr
derefDescr = (derefCode, closedSig (Arrow (Times N (Arrow N N)) N))

assignDescr :: TransducerDescr
assignDescr = (assignCode, closedSig (Arrow (Times (Times N (Arrow N N)) N) N))
