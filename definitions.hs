module Definitions where

import           Control.Monad.State.Lazy (StateT)
import           Data.Map                 (Map)

data Type = N | Times Type Type | Arrow Type Type deriving (Eq, Show)
data Signature = Signature (Map String Type) Type deriving (Eq, Show)

type Variable = (String, Type)

data AST = Var String
         | App AST AST
         | Abs Variable AST
         | IfThenElse AST AST AST
         | IConst Int
         | BinOp String AST AST
         | Local Variable AST
         | Ref AST
         | Assign AST AST
         | Sequential AST AST
         | While AST AST
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


type EvalState = StateT (Int, [String]) (Either String)
