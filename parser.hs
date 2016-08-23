import Data.Char (isAlpha, isDigit)
import Control.Applicative

operators :: String
operators = "+-*/%><()!;=\\"

longOperators :: [String]
longOperators = ["->", ":=", "||", "&&", "==", "!="]

forbiddenNames :: [String]
forbiddenNames = ["if", "then", "else", "while", "do", "True", "False", "new", "label", "semaphore", "let", "in", "break", "continue", "grab", "release"]

data Token = TOp String
           | TWord String
           | TInt Int
           deriving (Eq, Show)

tokenize, tokenize' :: String -> [Token]
tokenize "" = []
tokenize [c] = tokenize' [c]
tokenize ('|' : '|' : '|' : cs) = TOp "|||" : tokenize cs
tokenize s@(c1 : c2 : cs) | [c1, c2] `elem` longOperators = TOp [c1, c2] : tokenize cs
                          | otherwise                      = tokenize' s

tokenize' "" = [] -- not reached
tokenize' str@(c : cs) | c `elem` operators = TOp [c] : tokenize cs
                       | not (null i)       = TInt (read i) : tokenize is
                       | not (null s)       = TWord s : tokenize ss
                       | otherwise          = tokenize cs
  where
    (i, is) = span isDigit str
    (s, ss) = span isAlpha str



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

type Nip = [Token] -> Maybe (AST, [Token])

-- returns (unfoldr go ini, last (failed) ini)
unfoldrPlus :: (b -> Maybe (a, b)) -> b -> ([a], b)
unfoldrPlus go ini = case go ini of Nothing     -> ([], ini) 
                                    Just (a, b) -> let (xs, r) = unfoldrPlus go b in (a : xs, r)

collect :: [String] -> (String -> AST -> AST -> AST) -> Nip -> Nip
collect ops glue argNip s = do
    (x0, r) <- argNip s
    case collectGo x0 r of Just (t, r') -> return (t, r')
                           Nothing      -> return (x0, r)
    where
        collectGo acc ts = do
            (TOp c : s1) <- return ts
            True <- return $ c `elem` ops
            (x, r) <- argNip s1
            case collectGo (glue c acc x) r of Just (t, r') -> return (t, r')
                                               Nothing      -> return (glue c acc x, r)

oneOf :: [Nip] -> Nip
oneOf xs s = foldr1 (<|>) $ map ($ s) xs

withoutSugar :: [Token] -> [Token]
withoutSugar = helper False 
    where 
        helper _ [] = []
        helper False (TOp "\\" : ts) = helper True ts
        helper False (t : ts) = t : helper False ts
        helper True (TOp "->" : ts) = helper False ts
        helper True (t : ts) = TOp "\\" : t : TOp "->" : helper True ts



nipProg :: Nip
nipProg = oneOf [nipAbs, nipLet, nipLocal, nipLabel, nipSemaphore, nipBlock]

nipAbs :: Nip
nipAbs s = do
    (TOp "\\" : TWord x : TOp "->" : r) <- return s
    (t, r') <- nipProg r
    return (Abs x t, r')

nipLet :: Nip
nipLet s = do
    (TWord "let" : TWord x : TOp "=" : s1) <- return s
    (e, s2) <- nipExpr s1
    (TWord "in" : s3) <- return s2
    (t, r) <- nipProg s3
    return (App (Abs x t) e, r)

nipLocal :: Nip
nipLocal s = do
    (TWord "new" : TWord x : TWord "in" : r) <- return s
    (t, r') <- nipProg r
    return (Local x t, r')

nipLabel :: Nip
nipLabel s = do
    (TWord "label" : TWord x : TWord "in" : r) <- return s
    (t, r') <- nipProg r
    return (Label x t, r')

nipSemaphore :: Nip
nipSemaphore s = do
    (TWord "semaphore" : TWord x : TWord "in" : r) <- return s
    (t, r') <- nipProg r
    return (Semaphore x t, r')

nipBlock :: Nip
nipBlock = collect [";"] (\_ x y -> Sequential x y) nipInstruct

nipInstruct :: Nip
nipInstruct = oneOf [nipWhile, nipSimpleInstruct]

nipWhile :: Nip
nipWhile s =  do
    (TWord "while" : s1) <- return s
    (c, s2) <- nipExpr s1
    (TWord "do" : s3) <- return s2
    (a, r) <- nipSimpleInstruct s3
    return (While c a, r)

nipSimpleInstruct :: Nip
nipSimpleInstruct = collect ["|||"] (\_ x y -> Parallel x y) nipCommand

nipCommand :: Nip
nipCommand = oneOf [nipAssign, nipBreak, nipContinue, nipGrab, nipRelease, nipExpr]


nipBreak :: Nip
nipBreak s = do
    (TWord "break" : s') <- return s
    (t, r) <- nipExpr s'
    return (Break t, r)

nipContinue :: Nip
nipContinue s = do
    (TWord "continue" : s') <- return s
    (t, r) <- nipExpr s'
    return (Continue t, r)

nipGrab :: Nip
nipGrab s = do
    (TWord "grab" : s') <- return s
    (t, r) <- nipExpr s'
    return (Grab t, r)

nipRelease :: Nip
nipRelease s = do
    (TWord "release" : s') <- return s
    (t, r) <- nipExpr s'
    return (Release t, r)

nipAssign :: Nip
nipAssign s = do
    (x, s1) <- nipExpr s
    (TOp ":=" : s2) <- return s1
    (v, r) <- nipExpr s2
    return (Assign x v, r)

nipExpr :: Nip
nipExpr = oneOf [nipIf, nipBoolExpr]

nipIf :: Nip
nipIf s = do
    (TWord "if" : s1) <- return s
    (c, s2) <- nipExpr s1
    (TWord "then" : s3) <- return s2
    (t, s4) <- nipExpr s3
    (TWord "else" : s5) <- return s4
    (f, r) <- nipExpr s5
    return (IfThenElse c t f, r)

nipBoolExpr :: Nip
nipBoolExpr = collect ["||"] BinOp nipDisjunct

nipDisjunct :: Nip
nipDisjunct = collect ["&&"] BinOp nipConjunct

nipConjunct :: Nip
nipConjunct = oneOf [nipRelation, nipArithmExpr]

nipRelation :: Nip
nipRelation s = let ordOp = ["==", "!=", "<", ">"]
                in case collect ordOp BinOp nipArithmExpr s of Nothing               -> Nothing
                                                               Just (BinOp o x y, r) -> if o `elem` ordOp then Just (BinOp o x y, r) else Nothing
                                                               _                     -> Nothing

nipArithmExpr :: Nip
nipArithmExpr = collect ["+", "-"] BinOp nipAddend

nipAddend :: Nip
nipAddend = collect ["*", "/", "%"] BinOp nipMultiplier

nipMultiplier :: Nip
nipMultiplier s = let (fs, r) = unfoldrPlus nipFactor s
                  in if null fs then Nothing else Just $ (foldl1 App fs, r)

nipFactor :: Nip
nipFactor = oneOf [nipRef, nipInner]

nipRef :: Nip
nipRef s = do
    (TOp "!" : s') <- return s
    (t, r) <- nipInner s'
    return (Ref t, r)

nipInner :: Nip
nipInner = oneOf [nipInt, nipBool, nipFix, nipInnerProg, nipVal]

nipVal :: Nip
nipVal s = do
    (TWord x : r) <- return s
    False <- return $ x `elem` forbiddenNames 
    return (Var x, r)

nipBool :: Nip
nipBool s = do
    (TWord w : r) <- return s
    if w == "True"
        then return (BConst True, r)
        else if w == "False"
            then return (BConst False, r) 
            else fail ""

nipInt :: Nip
nipInt s = do
    (TInt n : r) <- return s
    return (IConst n, r)

nipFix :: Nip
nipFix s = do
    (TWord "fix" : r) <- return s
    return (Fix, r)

nipInnerProg :: Nip
nipInnerProg s = do
    (TOp "(" : s') <- return s
    (e, r) <- nipProg s'
    (TOp ")" : r') <- return r
    return (e, r')

parse :: String -> Maybe AST
parse str = (nipProg $ withoutSugar $ tokenize str) >>= (\(t, s) -> if null s then Just t else Nothing)

main :: IO ()
main = do
  prog <- readFile "example.ica"
  print (parse prog)