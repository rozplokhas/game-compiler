module Main where

import Data.Maybe
import Parser
import Collect
import Definitions
import Transducer
import System.IO

main :: IO ()
main = do
    inp <- openFile "example.sra" ReadMode
    outp <- openFile "result.c" WriteMode
    text <- hGetContents inp
    pr <- return $ fromJust $ parse text
    lt <- return $ transform pr
    tr <- return $ buildTransducer lt
    result <- return $ (\(Right x) -> x) $ runEval $ tr >>= generateCode
    hPutStr outp result
    hClose inp
    hClose outp
