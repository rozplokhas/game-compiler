module Rules (productRule, abstractionRule, applicationRule) where

import           Control.Monad (liftM2, liftM3)
import qualified Data.Map      as M (Map, delete, insert, intersection, keys,
                                     map, member, union, (!))
import           Definitions
import           Text.Printf   (printf)
import           Transducer
import           Utils         (copyCat, evalError, getFreshInt, prepend, typesMatch)


productRule :: Transducer -> Transducer -> EvalState Transducer
productRule tx ty = do
    inp <- inpState
    return $ Transducer inp outp printer
    where
        inpState = mapM freshWire $ M.union (inputWires tx) (inputWires ty)
        outp = WTimes (outputWire tx) (outputWire ty)
        printer tr@(Transducer inpW (WTimes wx wy) _) = liftM3 (\a b c -> a . b . c)
                                                            (switcher inpW (inputWires tx) (inputWires ty))
                                                            (code tx tr{inputWires = inputWires tx, outputWire = wx})
                                                            (code ty tr{inputWires = inputWires ty, outputWire = wy})

abstractionRule :: String -> Transducer -> EvalState Transducer
abstractionRule v t = return $ Transducer inp outp printer
    where
        inp = M.delete v (inputWires t)
        outp = WArrow (inputWires t M.! v) (outputWire t)
        printer tr@(Transducer inpW (WArrow wx wm) _) = code t tr{inputWires = M.insert v wx inpW, outputWire = wm}


applicationRule :: Transducer -> Transducer -> EvalState Transducer
applicationRule tf tx = do
    case (outputWire tf, outputWire tx) of 
        (WArrow w1 w2, wx) -> if typesMatch w1 wx
                                then result
                                else err
        otherwise -> err
    where
        result = if null (M.intersection (inputWires tf) (inputWires tx))
                 then return $ Transducer inp outp printer
                 else evalError "Error: intersecting contexts in application"
        inp = M.union (inputWires tf) (inputWires tx)
        outp = case outputWire tf of WArrow w w' -> w'
        printer tr@(Transducer inpW outpW c) =
            let cf = M.intersection inpW (inputWires tf) in
            let cx = M.intersection inpW (inputWires tx) in
            let bridge = outputWire tx in
            liftM2 (.) (code tf (Transducer cf (WArrow bridge outpW) c)) (code tx (Transducer cx bridge c))
        err = evalError "Error: mismatched types"


switchWire :: Wire -> Wire -> Wire -> Int -> ShowS
switchWire = switchWire' False

switchWire' :: Bool -> Wire -> Wire -> Wire -> Int -> ShowS
switchWire' False (WPort (q, n)) (WPort (q', n')) (WPort (q'', n'')) ind = prepend $ printf template
                                                                                     q' ind (0 :: Int) q q'' ind (1 :: Int) q n ind n'' n'
switchWire' True  (WPort (q, n)) (WPort (q', n')) (WPort (q'', n'')) ind = prepend $ printf template
                                                                                     n' ind (0 :: Int) n n'' ind (1 :: Int) n q ind q'' q'
switchWire' f     (WTimes w1 w2) (WTimes w1' w2') (WTimes w1'' w2'') ind = switchWire' f w1 w1' w1'' ind .
                                                                           switchWire' f w2 w2' w2'' ind
switchWire' f     (WArrow w1 w2) (WArrow w1' w2') (WArrow w1'' w2'') ind = switchWire' (not f) w1 w1' w1'' ind .
                                                                           switchWire' f       w2 w2' w2'' ind

template :: String
template = "\
\%s:\n\
\    mem[%d] = %d;\n\
\    goto %s;\n\
\%s:\n\
\    mem[%d] = %d;\n\
\    goto %s;\n\
\%s:\n\
\    if (mem[%d])\n\
\        goto %s;\n\
\    else\n\
\        goto %s;\n"

switcher :: M.Map String Wire -> M.Map String Wire -> M.Map String Wire -> EvalState ShowS
switcher sharedC leftC rightC | null sharedC = return id
                              | null leftC   = return $ foldr (.) id $ map (\x -> copyCat (rightC M.! x) (sharedC M.! x)) $ M.keys rightC
                              | null rightC  = return $ foldr (.) id $ map (\x -> copyCat (leftC  M.! x) (sharedC M.! x)) $ M.keys leftC 
                              | otherwise    = do 
    i <- getFreshInt
    return $ foldr (.) id $ map (helper i) $ M.keys sharedC
    where
        helper i x | M.member x leftC && M.member x rightC = switchWire (sharedC M.! x) (leftC M.! x) (rightC M.! x) i
                   | M.member x leftC                      = copyCat (leftC M.! x)  (sharedC M.! x)
                   | otherwise                             = copyCat (rightC M.! x) (sharedC M.! x)
