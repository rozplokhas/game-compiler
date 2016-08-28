module Utils where

import           Control.Monad.Except     (throwError)
import           Control.Monad.State.Lazy (evalStateT, get, put)
import qualified Data.Map                 as M (empty)
import           Definitions
import Text.Printf (printf)


closedSig :: Type -> Signature
closedSig = Signature M.empty

refType :: Type
refType = Times N (Arrow N N)


prepend :: String -> ShowS
prepend = (++)


evalError :: String -> EvalState a
evalError = throwError

getFreshString :: EvalState String
getFreshString = do
    (n, s : ss) <- get
    put (n, ss)
    return s

getFreshInt :: EvalState Int
getFreshInt = do
    (n, s) <- get
    put (n + 1, s)
    return n

runEval :: EvalState a -> Either String a
runEval es = evalStateT es (0, allStrings)
    where
        allStrings = concat $ tail $ iterate (\l -> (:) <$> letters <*> l) [""]
        letters = ['a'..'z']

copyCat :: Wire -> Wire -> ShowS
copyCat = copyCat' False
    where
        copyCat' False (WPort (q, n)) (WPort (q', n')) = prepend $ printf template q q' n' n
        copyCat' True  (WPort (q, n)) (WPort (q', n')) = prepend $ printf template q' q n n'
        copyCat' f     (WTimes w1 w2) (WTimes w1' w2') = copyCat' f       w1 w1' . copyCat' f w2 w2'
        copyCat' f     (WArrow w1 w2) (WArrow w1' w2') = copyCat' (not f) w1 w1' . copyCat' f w2 w2'

        template = "\
\%s:\n\
\    goto %s;\n\
\%s:\n\
\    goto %s;\n"

typesMatch :: Wire -> Wire -> Bool
typesMatch (WPort _)      (WPort _)        = True
typesMatch (WTimes w1 w2) (WTimes w1' w2') = typesMatch w1 w1' && typesMatch w2 w2'
typesMatch (WArrow w1 w2) (WArrow w1' w2') = typesMatch w1 w1' && typesMatch w2 w2'
typesMatch _              _                = False
