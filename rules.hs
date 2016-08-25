module Rules where

import qualified Data.Map as M (union, intersection, delete, (!), insert, singleton)
import Definitions
import Transducer
import ConstTransducers

prod :: Transducer -> Transducer -> EvalState Transducer
prod tx ty = return $ Transducer inp outp printer
    where
        inp = M.union (inputWires tx) (inputWires ty)
        outp = WTimes (outputWire tx) (outputWire ty)
        printer tr@(Transducer _  (WTimes wx wy) _) = (code tx tr{outputWire = wx}) . (code ty tr{outputWire = wy})

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
            (code tf (Transducer cf (WArrow bridge outpW) c)) . (code tx (Transducer cx bridge c))


-- Code for "(\x -> x) 1"
test :: EvalState String
test = do
    v <- createTransducer (variableCode "x", Signature (M.singleton "x" N) N)
    res <- abstraction "x" v
    one <- createTransducer (numberDescr 1)
    ap <- application res one
    return $ generateCode  ap
