module Transform (transform, buildTransducer) where

import           ConstTransducers
import           Data.Map         as M (Map, empty, insert, lookup, singleton)
import           Definitions
import           Rules
import           Transducer
import           Utils            (evalError)

data LambdaTree = LVar String
                | LApp LambdaTree LambdaTree
                | LAbs Variable LambdaTree
                | LProduct LambdaTree LambdaTree
                | LConst TransducerDescr

transform :: AST -> LambdaTree
transform (Var s)            = LVar s
transform (App f x)          = LApp (transform f) (transform x)
transform (Abs x t)          = LAbs x (transform t)
transform (IfThenElse c t f) = LApp (LConst ifDescr) (LProduct (LProduct (transform c) (transform t)) (transform f))
transform (IConst i)         = LConst (numberDescr i)
transform (BinOp op x y)     = LApp (LConst (binOpDescr op)) (LProduct (transform x) (transform y))
transform (Local x t)        = LApp (LAbs x (transform t)) (LConst newVarDescr)
transform (Ref t)            = LApp (LConst derefDescr) (transform t)
transform (Assign t1 t2)     = LApp (LConst assignDescr) (LProduct (transform t1) (transform t2))
transform (Sequential t1 t2) = LApp (LConst seqDescr) (LProduct (transform t1) (transform t2))
transform (While t1 t2)      = LApp (LConst whileDescr) (LProduct (transform t1) (transform t2))



buildTransducer :: LambdaTree -> EvalState Transducer
buildTransducer = buildTransducer' empty

buildTransducer' :: M.Map String Type -> LambdaTree -> EvalState Transducer
buildTransducer' context (LVar x) =
    case M.lookup x context of Nothing -> evalError $ "Error: undeclared identifier " ++ x
                               Just t  -> createTransducer (variableCode x, Signature (singleton x t) t)

buildTransducer' context (LApp f x) = do
    ft <- buildTransducer' context f
    xt <- buildTransducer' context x
    applicationRule ft xt

buildTransducer' context (LAbs (x, t) m) = do
    mt <- buildTransducer' (insert x t context) m
    abstractionRule x mt

buildTransducer' context (LProduct a b) = do
    at <- buildTransducer' context a
    bt <- buildTransducer' context b
    productRule at bt

buildTransducer' _ (LConst descr) = do
    createTransducer descr
