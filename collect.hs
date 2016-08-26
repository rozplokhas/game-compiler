module Collect (transform, buildTransducer) where

import           ConstTransducers
import           Data.Map         as M (Map, singleton, insert, empty, (!))
import           Definitions
import           Rules
import           Transducer


data LambdaTree = LVar String
                | LApp LambdaTree LambdaTree
                | LAbs Variable LambdaTree
                | LProduct LambdaTree LambdaTree
                | LConst TransducerDescr

transform :: AST -> LambdaTree
transform (Var s) = LVar s
transform (App f x) = LApp (transform f) (transform x)
transform (Abs x t) = LAbs x (transform t)
transform (IfThenElse c t f) = LApp (LConst ifDescr) (LProduct (LProduct (transform c) (transform t)) (transform f))
transform (IConst i) = LConst (numberDescr i)
transform (BinOp op x y) = LApp (LConst (binOpDescr op)) (LProduct (transform x) (transform y))
transform (Local x t) = LApp (LAbs x (transform t)) (LConst newVarDescr)
transform (Ref t) = LApp (LConst derefDescr) (transform t)
transform (Assign t1 t2) = LApp (LConst assignDescr) (LProduct (transform t1) (transform t2))
transform (Sequential t1 t2) = LApp (LConst seqDescr) (LProduct (transform t1) (transform t2))
transform (While t1 t2) = LApp (LConst whileDescr) (LProduct (transform t1) (transform t2))



buildTransducer :: LambdaTree -> EvalState Transducer
buildTransducer = buildTransducer' empty

buildTransducer' :: M.Map String Type -> LambdaTree -> EvalState Transducer

buildTransducer' context (LVar x) = 
    let t = context ! x 
    in createTransducer (variableCode x, Signature (singleton x t) t)

buildTransducer' context (LApp f x) = do
    ft <- buildTransducer' context f
    xt <- buildTransducer' context x
    application ft xt

buildTransducer' context (LAbs (x, t) m) = do
    mt <- buildTransducer' (insert x t context) m
    abstraction x mt

buildTransducer' context (LProduct a b) = do
    at <- buildTransducer' context a
    bt <- buildTransducer' context b
    prod at bt

buildTransducer' context (LConst descr) = do
    createTransducer descr
