module Main where

import Parser
import Transform
import Transducer (generateCode)
import System.Environment (getArgs)
import Utils (runEval)
import Data.List (findIndices)
import Optimizer

cutExtention :: String -> String
cutExtention s = case findIndices (== '.') s of [] -> s
                                                xs -> take (last xs) s

main :: IO ()
main = do
    args <- getArgs
    case args of [fileName] -> normalWay fileName
                 otherwise  -> putStrLn "Programm expects one argument"
    where 
        normalWay fileName = do
            text <- readFile fileName
            let pr = parse text
            let lt = transform <$> pr
            let tr = lt >>= buildTransducer
            let result = tr >>= generateCode
            let optResult = runEval $ result >>= optimize
            case optResult of Left err   -> putStrLn err
                              Right code -> writeFile (cutExtention fileName ++ ".c") code
