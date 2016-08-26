module Rules where

import           Control.Monad (liftM2, liftM3)
import qualified Data.Map      as M (Map, delete, insert, intersection, union, (!), keys, member, map)
import           Definitions
import           Transducer
import Text.Printf (printf)

prod :: Transducer -> Transducer -> EvalState Transducer
prod tx ty = do
    inp <- inpState
    return $ Transducer inp outp printer
    
    where
        inpState = mapM freshWire $ M.union (inputWires tx) (inputWires ty)
        outp = WTimes (outputWire tx) (outputWire ty)
        printer tr@(Transducer inpW (WTimes wx wy) _) = liftM3 (\a b c -> a . b . c) 
                                                            (switcher inpW (inputWires tx) (inputWires ty)) 
                                                            (code tx tr{inputWires = inputWires tx, outputWire = wx}) 
                                                            (code ty tr{inputWires = inputWires ty, outputWire = wy})

abstraction :: String -> Transducer -> EvalState Transducer
abstraction v t = return $ Transducer inp outp printer
    where
        inp = M.delete v (inputWires t)
        outp = WArrow (inputWires t M.! v) (outputWire t)
        printer tr@(Transducer inpW (WArrow wx wm) _) = code t tr{inputWires = M.insert v wx inpW, outputWire = wm}


application :: Transducer -> Transducer -> EvalState Transducer
application tf tx =
    if null (M.intersection (inputWires tf) (inputWires tx))
        then
            return $ Transducer inp outp printer
        else
            fail "Error: intersecting contextes in application"
    where
        inp = M.union (inputWires tf) (inputWires tx)
        outp = case outputWire tf of WArrow w w' -> w'
        printer tr@(Transducer inpW outpW c) =
            let cf = M.intersection inpW (inputWires tf) in
            let cx = M.intersection inpW (inputWires tx) in
            let bridge = outputWire tx in
            liftM2 (.) (code tf (Transducer cf (WArrow bridge outpW) c)) (code tx (Transducer cx bridge c))

switchWireQ :: Bool -> Wire -> Wire -> Wire -> Int -> String
switchWireQ False (WPort (qi, _)) (WPort (ql, _)) (WPort (qr, _)) ind = printf "\
\%s:\n\
\    mem[%d] = %d;\n\
\    goto %s;\n\
\%s:\n\
\    mem[%d] = %d;\n\
\    goto %s;\n" ql ind (0 :: Int) qi qr ind (1 :: Int) qi
switchWireQ True (WPort (qi, _))  (WPort (ql, _)) (WPort (qr, _)) ind = printf "\
\%s:\n\
\    if (mem[%d])\n\
\        goto %s;\n\
\    else\n\
\        goto %s;\n" qi ind qr ql
switchWireQ f (WTimes iwl iwr) (WTimes owl owr) (WTimes owl' owr') ind = switchWireQ f iwl owl owl' ind       ++ switchWireQ f iwr owr owr' ind
switchWireQ f (WArrow iwl iwr) (WArrow owl owr) (WArrow owl' owr') ind = switchWireQ (not f) iwl owl owl' ind ++ switchWireQ f iwr owr owr' ind

switchWireA :: Bool -> Wire -> Wire -> Wire -> Int -> String
switchWireA False (WPort (_, ni))  (WPort (_, nl)) (WPort (_, nr)) ind = printf "\
\%s:\n\
\    if (mem[%d])\n\
\        goto %s;\n\
\    else\n\
\        goto %s;\n" ni ind nr nl
switchWireA True (WPort (_, ni))  (WPort (_, nl)) (WPort (_, nr)) ind = printf "\
\%s:\n\
\    mem[%d] = %d;\n\
\    goto %s;\n\
\%s:\n\
\    mem[%d] = %d;\n\
\    goto %s;\n" nl ind (0 :: Int) ni nr ind (1 :: Int) ni
switchWireA f (WTimes iwl iwr) (WTimes owl owr) (WTimes owl' owr') ind = switchWireA f       iwl owl owl' ind ++ switchWireA f iwr owr owr' ind
switchWireA f (WArrow iwl iwr) (WArrow owl owr) (WArrow owl' owr') ind = switchWireA (not f) iwl owl owl' ind ++ switchWireA f iwr owr owr' ind

copyCatQ :: Bool -> Wire -> Wire -> String
copyCatQ False (WPort (qi, _)) (WPort (qo, _)) = printf "\
\%s:\n\
\    goto %s;\n" qo qi
copyCatQ True (WPort (qi, _)) (WPort (qo, _)) = printf "\
\%s:\n\
\    goto %s;\n" qi qo
copyCatQ f (WTimes iwl iwr) (WTimes owl owr) = copyCatQ f iwl owl ++ copyCatQ f iwr owr
copyCatQ f (WArrow iwl iwr) (WArrow owl owr) = copyCatQ (not f) iwl owl ++ copyCatQ f iwr owr

switchContextQ :: M.Map String Wire -> M.Map String Wire -> M.Map String Wire -> Int -> String
switchContextQ iC lC rC ind = 
    concat $ map helper $ M.keys iC
        where
            helper x | M.member x lC && M.member x rC = switchWireQ False (iC M.! x) (lC M.! x) (rC M.! x) ind
                     | M.member x lC                  = copyCatQ False (iC M.! x) (lC M.! x)
                     | otherwise                      = copyCatQ False (iC M.! x) (rC M.! x)

copyCatA :: Bool -> Wire -> Wire -> String
copyCatA False (WPort (_, ni)) (WPort (_, no)) = printf "\
\%s:\n\
\    goto %s;\n" ni no
copyCatA True (WPort (_, ni)) (WPort (_, no)) = printf "\
\%s:\n\
\    goto %s;\n" no ni
copyCatA f (WTimes iwl iwr) (WTimes owl owr) = copyCatA f iwl owl ++ copyCatA f iwr owr
copyCatA f (WArrow iwl iwr) (WArrow owl owr) = copyCatA (not f) iwl owl ++ copyCatA f iwr owr

switchContextA :: M.Map String Wire -> M.Map String Wire -> M.Map String Wire -> Int -> String
switchContextA iC lC rC ind = 
    concat $ map helper $ M.keys iC
        where
            helper x | M.member x lC && M.member x rC = switchWireA False (iC M.! x) (lC M.! x) (rC M.! x) ind
                     | M.member x lC                  = copyCatA False (iC M.! x) (lC M.! x)
                     | otherwise                      = copyCatA False (iC M.! x) (rC M.! x)

switcher :: M.Map String Wire -> M.Map String Wire -> M.Map String Wire -> EvalState ShowS
switcher sharedC leftC rightC | null sharedC = return id
                              | otherwise = do
    i <- getFreshInt
    return $ prepend $ switchContextQ sharedC leftC rightC i ++ switchContextA sharedC leftC rightC i