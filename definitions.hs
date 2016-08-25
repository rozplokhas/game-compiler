module Definitions where

import           Control.Monad.State.Lazy (StateT, evalStateT, get, put)
import           Data.Map                 (Map, empty)

data AST = Var String
         | App AST AST
         | Abs String AST
         | IfThenElse AST AST AST
         | BConst Bool
         | IConst Int
         | Fix
         | BinOp String AST AST
         | Local String AST
         | Ref AST
         | Assign AST AST
         | Sequential AST AST
         | Label String AST
         | Break AST
         | Continue AST
         | While AST AST
         | Parallel AST AST
         | Semaphore String AST
         | Grab AST
         | Release AST
         deriving (Eq, Show)

data Type = N | Times Type Type | Arrow Type Type deriving (Eq, Show)
data Signature = Signature (Map String Type) Type deriving (Eq, Show)

closedSig :: Type -> Signature
closedSig = Signature empty

type Label = String
type Port = (Label, Label)
data Wire = WPort Port | WTimes Wire Wire | WArrow Wire Wire deriving (Eq, Show)

type CodePrinter = Transducer -> ShowS
type TransducerDescr = (CodePrinter, Signature)
data Transducer = Transducer {
                    inputWires :: Map String Wire,
                    outputWire :: Wire,
                    code       :: CodePrinter
                  }

type EvalState = StateT [Label] (Either String)

allStrings :: [String]
allStrings = concat $ tail $ iterate (\l -> (:) <$> letters <*> l) [""]
    where letters = ['a'..'z']

getFreshString :: EvalState String
getFreshString = do
    (s : ss) <- get
    put ss
    return s

runEval :: EvalState a -> Either String a
runEval es = evalStateT es allStrings
