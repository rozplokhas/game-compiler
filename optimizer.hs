module Optimizer (optimize) where

import qualified Data.Map    as M (Map, empty, insert, adjust, keys, member, notMember,
                                   (!), foldrWithKey, filter, filterWithKey, map, fromList)
import           Definitions (Label, EvalState)
import qualified Data.Set as S (Set, empty, insert, member, notMember, elems)
import Utils (evalError, prepend, runEval)
import Text.Printf (printf)


data Edge = Empty Label
          | Substantive String Label
          | Choise String Label Label
          deriving (Eq, Show)

type Edges = M.Map Label Edge

data Graph = Graph {
    edges  :: Edges, 
    start  :: Label,
    finish :: Label
} deriving (Eq, Show)



labelFromGoto :: [String] -> Label
labelFromGoto ws = init $ ws !! 1

insertEdge :: Label -> Edge -> Graph -> Graph
insertEdge l e g = g{edges = M.insert l e (edges g)}

spanHead :: [String] -> ([String], [String])
spanHead = span notGoto
    where
        notGoto str = let ws = words str in null ws || head ws /= "goto"


buildGraph, buildGraph', getFinish :: [[String]] -> Graph
buildGraph (st : ss) = let g = buildGraph' (dropWhile null ss) in g{start = labelFromGoto st}

buildGraph' ([] : ss) = getFinish (dropWhile null ss)
buildGraph' (ws : ws2 : ss) | head ws2 == "goto" = let g = buildGraph' ss
                                                   in insertEdge label (Empty $ labelFromGoto ws2) g
                            | head ws2 == "if"   = let (ts : _ : es : rs, g) = (ss, buildGraph' rs)
                                                   in insertEdge label (Choise (init $ tail $ ws2 !! 1) 
                                                                               (labelFromGoto ts) (labelFromGoto es)) g
                            | otherwise          = let (gs : rs, g) = (ss, buildGraph' rs)
                                                   in insertEdge label (Substantive (unwords ws2) (labelFromGoto gs)) g
    where
        label = init (head ws)

getFinish (fin : ss) = Graph M.empty "" (init $ head $ fin)

graphFromString :: String -> (String, Graph)
graphFromString str = let (hd, rest) = spanHead (lines str)
                      in (unlines hd, buildGraph $ map words $ rest)



revEdges :: Graph -> M.Map Label [Label]
revEdges Graph{edges = es} = M.foldrWithKey upd M.empty es
    where 
        upd k (Empty v)         m = addEdge v k m
        upd k (Substantive _ v) m = addEdge v k m
        upd k (Choise _ t f)    m = addEdge t k $ addEdge f k m
        addEdge v u m = if v `M.member` m then M.adjust (u :) v m else M.insert v [u] m

finishing :: Graph -> S.Set Label
finishing g = dfs (finish g) S.empty
    where
        rEdges = revEdges g
        
        dfs :: Label -> S.Set Label -> S.Set Label
        dfs v visited | v `S.member`   visited = visited
                      | v `M.notMember` rEdges = S.insert v visited
                      | otherwise              = foldr dfs (S.insert v visited) $ rEdges M.! v

onlyFinishing :: Graph -> Graph
onlyFinishing gr = gr{edges = M.map correct $ M.filterWithKey (\k _ -> k `S.member` grFinishing) $ edges gr}
    where
        grFinishing = finishing gr
        correct e@(Choise c t f) | t `S.notMember` grFinishing = Empty f
                                 | f `S.notMember` grFinishing = Empty t
                                 | otherwise                   = e
        correct e = e

findRoot :: Edges -> Label -> (Label, Edges)
findRoot em v | v `M.notMember` em = (v, em)
              | otherwise          =  case em M.! v of Empty u -> let (fin, em') = findRoot em u
                                                                  in (fin, M.insert v (Empty fin) em')
                                                       otherwise -> (v, em)

constrict :: Edges -> Edges
constrict em = foldr (\v e -> snd $ findRoot e v) em $ M.keys em

newName :: Edges -> Label -> Label
newName em l | l `M.notMember` em = l
             | otherwise          = case em M.! l of Empty u -> u
                                                     otherwise -> l

rename :: Edges -> Edges
rename em = M.map rename' em
    where
        rename' (Substantive inf l) = Substantive inf (newName em l)
        rename' (Choise inf l1 l2) = Choise inf (newName em l1) (newName em l2)
        rename' e = e

delEmpties :: Graph -> Graph
delEmpties (Graph em st fin) = let em' = constrict em
                               in Graph (M.filter notEmpty $ rename em') (newName em' st) (newName em' fin)
    where
        notEmpty (Empty _) = False
        notEmpty _         = True

printGraph :: String -> Graph -> EvalState String
printGraph hd (Graph em st fin) =
    if st `M.notMember` em 
    then
        evalError "Your program will not terminate"
    else
        return $ hd ++
            printf "    goto %s;\n\n" st ++
            printEdges ++
            printf "\n\
\%s:\n\
\    %s\n\
\}\n" fin "printf(\"%d\\n\", acc);"
    where
        printEdges = M.foldrWithKey (\k e acc -> prependEdge k e . acc) id em ""
        
        prependEdge k (Empty l) = prepend $ printf "\
\%s:\n\
\    goto %s;\n" k l
        
        prependEdge k (Substantive inf l) = prepend $ printf "\
\%s:\n\
\    %s\n\
\    goto %s;\n" k inf l

        prependEdge k (Choise c t f) = prepend $ printf "\
\%s:\n\
\    if (%s)\n\
\        goto %s;\n\
\    else\n\
\        goto %s;\n" k c t f


extractIndices :: String -> [Either String Int]
extractIndices "" = []
extractIndices ('[' : s) = let (n, ']' : r) = span (/= ']') s
                           in Right (read n) : extractIndices r
extractIndices s = let (str, r) = span (/= '[') s
                   in Left str : extractIndices r

getIndMap :: [Either String Int] -> M.Map Int Int
getIndMap s = M.fromList $ zip (S.elems $ foldr op S.empty s) [0..]
    where
        op (Left s)  acc = acc
        op (Right n) acc = S.insert n acc

fitIndices :: String -> String
fitIndices str = concat $ map helper ss
    where
        ss = extractIndices str
        im = getIndMap ss
        helper (Left s)  = s
        helper (Right n) = "[" ++ (show $ im M.! n) ++ "]"

optimize :: String -> EvalState String
optimize prog = do
    let (hd, gr) = graphFromString prog
    opt <- printGraph hd (delEmpties $ onlyFinishing gr)
    return $ fitIndices opt

main :: IO ()
main = do
    (hd, gr) <- graphFromString <$> readFile "good_example.c"
    -- let (root, e') = findRoot (edges gr) "nay"
    let text = (\(Right x) -> x) $ runEval $ printGraph hd (delEmpties $ onlyFinishing gr)
    writeFile "other.c" (fitIndices text)
