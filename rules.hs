module Rules where

import qualified Data.Map    as M (delete, insert, intersection, union, (!))
import           Definitions
import           Transducer
import Control.Monad (liftM2)

prod :: Transducer -> Transducer -> EvalState Transducer
prod tx ty = return $ Transducer inp outp printer
    where
        inp = M.union (inputWires tx) (inputWires ty)
        outp = WTimes (outputWire tx) (outputWire ty)
        printer tr@(Transducer _  (WTimes wx wy) _) = liftM2 (.) (code tx tr{outputWire = wx}) (code ty tr{outputWire = wy})

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
