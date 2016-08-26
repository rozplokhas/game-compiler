module Definitions where

import           Control.Monad.State.Lazy (StateT, evalStateT, get, put)
import           Data.Map                 (Map, empty)

data Type = N | Times Type Type | Arrow Type Type deriving (Eq, Show)
data Signature = Signature (Map String Type) Type deriving (Eq, Show)

closedSig :: Type -> Signature
closedSig = Signature empty

type Variable = (String, Type)

data AST = Var String
         | App AST AST
         | Abs Variable AST
         | IfThenElse AST AST AST
         | BConst Bool
         | IConst Int
         | Fix
         | BinOp String AST AST
         | Local Variable AST
         | Ref AST
         | Assign AST AST
         | Sequential AST AST
         | Label Variable AST
         | Break AST
         | Continue AST
         | While AST AST
         | Parallel AST AST
         | Semaphore Variable AST
         | Grab AST
         | Release AST
         | VarHelp Variable 
         deriving (Eq, Show)

type Label = String
type Port = (Label, Label)
data Wire = WPort Port | WTimes Wire Wire | WArrow Wire Wire deriving (Eq, Show)

type CodePrinter = Transducer -> EvalState ShowS
type TransducerDescr = (CodePrinter, Signature)
data Transducer = Transducer {
                    inputWires :: Map String Wire,
                    outputWire :: Wire,
                    code       :: CodePrinter
                  }

prepend :: String -> ShowS
prepend s = (s ++)

type EvalState = StateT (Int, [Label]) (Either String)

allStrings :: [String]
allStrings = concat $ tail $ iterate (\l -> (:) <$> letters <*> l) [""]
    where letters = ['a'..'z']

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
