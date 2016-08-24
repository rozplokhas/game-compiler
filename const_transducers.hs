module ConstTransducers where

import qualified Data.Map    as M ((!))
import           Definitions
import           Text.Printf (printf)

prepend :: String -> ShowS
prepend s = (s ++)

numberCode :: Int -> CodePrinter
numberCode d Transducer{outputWire = WPort (q, n)} =
    prepend $ printf "\
\%s:\n\
\    acc = %d;\n\
\    goto %s;\n" q d n

binOpCode :: String -> CodePrinter
binOpCode op Transducer{outputWire = WArrow (WTimes (WPort (qx, nx)) (WPort (qy, ny))) (WPort (qr, nr))} =
    let loc = "x"
    in prepend $ printf "\
\%s:\n\
\    goto %s;\n\
\%s:\n\
\    %s = acc;\n\
\    goto %s;\n\
\%s:\n\
\    acc = acc %s %s;\n\
\    goto %s;\n" qr qx nx loc qy ny op loc nr

seqCode :: CodePrinter
seqCode Transducer{outputWire = WArrow (WTimes (WPort (qx, nx)) (WPort (qy, ny))) (WPort (qr, nr))} =
    prepend $ printf "\
\%s:\n\
\     goto %s;\n\
\%s:\n\
\     goto %s;\n\
\%s:\n\
\     goto %s;\n" qr qx nx qy ny nr

ifCode :: CodePrinter
ifCode Transducer{outputWire = WArrow (WTimes (WTimes (WPort (qc, nc)) (WPort (qt, nt))) (WPort (qf, nf))) (WPort (qr, nr))} =
    prepend $ printf "\
\%s:\n\
\    goto %s;\n\
\%s:\n\
\    if (acc == 0)\n\
\        goto %s;\n\
\    else\n\
\        goto %s;\n\
\%s:\n\
\    goto %s;\n\
\%s:\n\
\    goto %s;\n" qr qc nc qt qf nt nr nf nr

whileCode :: CodePrinter
whileCode Transducer{outputWire = WArrow (WTimes (WPort (qc, nc)) (WPort (qa, na))) (WPort (qr, nr))} =
    prepend $ printf "\
\%s:\n\
\    goto %s;\n\
\%s:\n\
\    if (acc == 0)\n\
\        goto %s;\n\
\    else\n\
\        goto %s;\n\
\%s:\n\
\    goto %s;\n" qr qc nc qa nr na qc

variableCode :: String -> CodePrinter
variableCode s Transducer{inputWires = inpW, outputWire = WPort (q, n)} =
    let (WPort (q', n')) = inpW M.! s
    in prepend $ printf "\
\%s:\n\
\    goto %s;\n\
\%s:\n\
\    goto %s;\n" q q' n' n

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
