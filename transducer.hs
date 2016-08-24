import           Control.Monad.State.Lazy
import           Data.Char                (isDigit)
import qualified Data.Map                 as M
import           Text.Printf


data Type = N | Times Type Type | Arrow Type Type deriving (Eq, Show)
data Signature = Signature [Type] Type deriving (Eq, Show)

type Label = String

type Port = (Label, Label)
data Wire = WPort Port | WTimes Wire Wire | WArrow Wire Wire deriving (Eq, Show)

type TransducerGoal = String
data Transducer = Transducer {
                    idNum      :: Int,
                    signature  :: Signature,
                    goal       :: TransducerGoal,
                    inputWires :: [Wire],
                    outputWire :: Wire
                  } deriving (Eq, Show)

type Env = State (Int, [String])

allStrings :: [String]
allStrings = concat $ tail $ iterate (\l -> (:) <$> letters <*> l) [""]
    where letters = ['a'..'z']

initEnv :: (Int, [String])
initEnv = (0, allStrings)

getFreshInt :: Env Int
getFreshInt = do
    (n, s) <- get
    put (n + 1, s)
    return n

getFreshString :: Env String
getFreshString = do
    (n, s : ss) <- get
    put (n, ss)
    return s

wireByType :: Type -> Env Wire
wireByType N = do
    s <- getFreshString
    return $ WPort ('q' : s, 'n' : s)
wireByType (Times t1 t2) = do
    w1 <- wireByType t1
    w2 <- wireByType t2
    return (WTimes w1 w2)
wireByType (Arrow t1 t2) = do
    w1 <- wireByType t1
    w2 <- wireByType t2
    return (WArrow w1 w2)

createTransducer :: TransducerGoal -> Signature -> Env Transducer
createTransducer goal sig@(Signature ts t) = do
    idNum <- getFreshInt
    outw <- wireByType t
    inw <- mapM wireByType ts
    return $ Transducer idNum sig goal inw outw


code :: Transducer -> String
code t@(Transducer {goal = g}) | not (null g) && isDigit (head g) = numberCode g t
                               | otherwise                        = (codePrinters M.! g) t

codePrinters :: M.Map TransducerGoal (Transducer -> String)
codePrinters = M.fromList [("add", addCode)]

numberCode :: String -> Transducer -> String
numberCode d Transducer {outputWire = WPort (q, n)} = printf "\
\%s:\n\
\   acc = %s;\n\
\   goto %s;\n" q d n

localX :: Int -> String
localX n = "x" ++ show n

addCode :: Transducer -> String
addCode Transducer {idNum = i, outputWire = WArrow (WTimes (WPort (qx, nx)) (WPort (qy, ny))) (WPort (qr, nr))} =
    let loc = localX i
    in printf "\
\%s:\n\
\    goto %s;\n\
\%s:\n\
\    %s = acc;\n\
\    goto %s;\n\
\%s:\n\
\    acc = acc + %s;\n\
\    goto %s;\n" qr qx nx loc qy ny loc nr
