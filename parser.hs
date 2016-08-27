module Parser (parse) where

import           Control.Applicative ((<|>))
import           Control.Monad       (liftM2)
import           Data.Char           (isAlpha, isDigit, isLower, isSpace,
                                      isUpper)
import           Data.List           (find)
import           Data.Maybe          (isJust)
import           Definitions

operators :: String
operators = "():!+-*/%<>;=\\"

longOperators :: [String]
longOperators = ["->", "==", "!=", "<=", ">=",  "&&", "||", ":="]

forbiddenNames :: [String]
forbiddenNames = ["if", "then", "else", "while", "do", "let", "new", "in", "N"]

data Token = TOp String
           | TWord String
           | TInt Int
           deriving (Eq, Show)

tokenize, tokenize' :: String -> EvalState [Token]
tokenize "" = return []
tokenize [c] = tokenize' [c]
tokenize s@(c1 : c2 : cs) | [c1, c2] `elem` longOperators = (TOp [c1, c2] :) <$> tokenize cs
                          | otherwise                     = tokenize' s
tokenize' "" = return [] -- unreachable
tokenize' str@(c : cs) | c `elem` operators    = (TOp [c] :) <$> tokenize cs
                       | c == '_'              = if isJust (find (/= '_') w)
                                                 then (TWord w :) <$> tokenize ws
                                                 else evalError $ "Parse error: unexpected token " ++ w
                       | isLower c || w == "N" = (TWord w :) <$> tokenize ws
                       | not (null i)          = (TInt (read i) :) <$> tokenize is
                       | not (null w)          = evalError $ "Parse error: identifiers can't start with upper case letter or digit"
                       | isSpace c             = tokenize cs
                       | otherwise             = evalError $ "Parse error: unexpected symbol " ++ [c]
  where
    (i, is) = span isDigit str
    (w, ws) = span (liftM2 (||) (liftM2 (||) isAlpha isDigit) (== '_')) str



type Nip a = [Token] -> Maybe (a, [Token])
type NipAST = Nip AST

oneOf :: [Nip a] -> Nip a
oneOf xs s = foldr1 (<|>) $ map ($ s) xs

collect :: Maybe [String] -> NipAST -> (String -> AST -> AST -> AST) -> NipAST
collect separators nipElement constructor = rec Nothing ""
    where
        withSep :: Maybe AST -> NipAST
        withSep acc = case separators of Nothing  -> rec acc ""
                                         Just ops -> nipSep ops acc

        nipSep :: [String] -> Maybe AST -> NipAST
        nipSep ops acc s = do
            (TOp op : r) <- return s
            True <- return $ op `elem` ops
            rec acc op r

        rec :: Maybe AST -> String -> NipAST
        rec acc op s = do
            (x, s1) <- nipElement s
            newAcc <- case acc of Nothing -> return x
                                  Just a  -> return $ constructor op a x
            case withSep (Just newAcc) s1 of Nothing     -> return (newAcc, s1)
                                             Just (a, r) -> return (a, r)


nipProg :: NipAST
nipProg = oneOf [nipAbs, nipLet, nipLocal, nipBlock]

nipAbs :: NipAST
nipAbs s = do
    (TOp "\\" : s1) <- return s
    (vs, s2) <- getAllVarsRev s1
    (TOp "->" : s3) <- return s2
    (t, r) <- nipProg s3
    return (foldr Abs t vs, r)
    where
        getAllVarsRev :: [Token] -> Maybe ([Variable], [Token])
        getAllVarsRev s' = do
            (v', s1') <- nipVarDef s'
            case getAllVarsRev s1' of Nothing        -> return ([v'], s1')
                                      Just (vs', r') -> return (v' : vs', r')


nipLet :: NipAST
nipLet s = do
    (TWord "let" : s1) <- return s
    (v, s2) <- nipVarDef s1
    (TOp "=" : s3) <- return s2
    (e, s4) <- nipProg s3
    (TWord "in" : s5) <- return s4
    (t, r) <- nipProg s5
    return (App (Abs v t) e, r)

nipLocal :: NipAST
nipLocal s = do
    (TWord "new" : s1) <- return s
    (Var name, s2) <- nipVar s1
    (TWord "in" : s3) <- return s2
    (t, r) <- nipProg s3
    return (Local (name, Times N (Arrow N N)) t, r)

nipBlock :: NipAST
nipBlock = collect (Just [";"]) nipInstruction (\_ x y -> Sequential x y)

nipInstruction :: NipAST
nipInstruction = oneOf [nipWhile, nipAction]

nipWhile :: NipAST
nipWhile s =  do
    (TWord "while" : s1) <- return s
    (c, s2) <- nipExpr s1
    (TWord "do" : s3) <- return s2
    (a, r) <- nipAction s3
    return (While c a, r)

nipAction :: NipAST
nipAction = oneOf [nipAssign, nipExpr]

nipAssign :: NipAST
nipAssign s = do
    (x, s1) <- nipExpr s
    (TOp ":=" : s2) <- return s1
    (v, r) <- nipExpr s2
    return (Assign x v, r)

nipExpr :: NipAST
nipExpr = oneOf [nipIf, nipArithmExpr]

nipIf :: NipAST
nipIf s = do
    (TWord "if" : s1) <- return s
    (c, s2) <- nipExpr s1
    (TWord "then" : s3) <- return s2
    (t, s4) <- nipExpr s3
    (TWord "else" : s5) <- return s4
    (f, r) <- nipExpr s5
    return (IfThenElse c t f, r)

nipArithmExpr :: NipAST
nipArithmExpr = oneOf [nipDisjunction, nipDisjunct]

nipDisjunction :: NipAST
nipDisjunction s = do
    (x, s1) <- nipDisjunct s
    (TOp "||" : s2) <- return s1
    (y, r) <- nipDisjunct s2
    return (BinOp "||" x y, r)

nipDisjunct :: NipAST
nipDisjunct = oneOf [nipConjunction, nipConjunct]

nipConjunction :: NipAST
nipConjunction s = do
    (x, s1) <- nipConjunct s
    (TOp "&&" : s2) <- return s1
    (y, r) <- nipConjunct s2
    return (BinOp "&&" x y, r)

nipConjunct :: NipAST
nipConjunct = oneOf [nipRelation, nipSum]

nipRelation :: NipAST
nipRelation s = do
    (x, s1) <- nipSum s
    (TOp op : s2) <- return s1
    True <- return $ op `elem` ["==", "!=", "<", ">", "<=", ">="]
    (y, r) <- nipSum s2
    return (BinOp op x y, r)

nipSum :: NipAST
nipSum = collect (Just ["+", "-"]) nipProduct BinOp

nipProduct :: NipAST
nipProduct = collect (Just ["*", "/", "%"]) nipApplication BinOp

nipApplication :: NipAST
nipApplication = collect Nothing nipValue (\_ x y -> App x y)

nipValue :: NipAST
nipValue = oneOf [nipRef, nipAtom]

nipRef :: NipAST
nipRef s = do
    (TOp "!" : s1) <- return s
    (t, r) <- nipAtom s1
    return (Ref t, r)

nipAtom :: NipAST
nipAtom = oneOf [nipInt, nipInnerProg, nipVar]

nipVar :: NipAST
nipVar s = do
    (TWord x : r) <- return s
    False <- return $ x `elem` forbiddenNames
    return (Var x, r)

nipInt :: NipAST
nipInt s = do
    (TInt n : r) <- return s
    return (IConst n, r)

nipInnerProg :: NipAST
nipInnerProg s = do
    (TOp "(" : s1) <- return s
    (pr, s2) <- nipProg s1
    (TOp ")" : r) <- return s2
    return (pr, r)

nipVarDef :: Nip Variable
nipVarDef s = do
    (Var name, s1) <- nipVar s
    (TOp ":" : s2) <- return s1
    (t, r) <- nipType s2
    return ((name, t), r)

nipType :: Nip Type
nipType s = do
    (t, s1) <- nipBaseType s
    case nipRest s1 of Nothing      -> return $ (t, s1)
                       Just (t2, r) -> return $ (Arrow t t2, r)
    where
        nipRest s' = do
            (TOp "->" : s1') <- return s'
            nipType s1'

nipBaseType :: Nip Type
nipBaseType = oneOf [nipInnerType, nipN]

nipInnerType :: Nip Type
nipInnerType s = do
    (TOp "(" : s1) <- return s
    (t, s2) <- nipType s1
    (TOp ")" : r) <- return s2
    return (t, r)

nipN :: Nip Type
nipN (TWord "N" : r) = return (N, r)
nipN _               = fail ""



parse :: String -> EvalState AST
parse text = do
    ts <- tokenize text
    case nipProg ts of Just (ast, []) -> return ast 
                       otherwise      -> evalError "Syntax error"