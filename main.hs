module Main where

import Parser
import Transform
import Transducer (generateCode)
import System.IO (openFile, hGetContents, hPutStr, hClose, IOMode (..))
import System.Environment (getArgs)
import Utils (runEval)
import Data.List (findIndices)

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
            inp <- openFile fileName ReadMode
            text <- hGetContents inp
            let pr = parse text
            let lt = transform <$> pr
            let tr = lt >>= buildTransducer
            let result = runEval $ tr >>= generateCode
            case result of Left err -> putStrLn err
                           Right code -> do
                                outp <- openFile (cutExtention fileName ++ ".c") WriteMode
                                hPutStr outp code
                                hClose outp
            hClose inp
