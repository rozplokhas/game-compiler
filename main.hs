module Main where

import Data.Maybe
import Parser
import Transform
import Transducer
import System.IO
import Utils (runEval)

main :: IO ()
main = do
    inp <- openFile "example.sra" ReadMode
    outp <- openFile "result.c" WriteMode
    text <- hGetContents inp
    pr <- return $ parse text
    lt <- return $ transform <$> pr
    tr <- return $ lt >>= buildTransducer
    result <- return $ (\(Right x) -> x) $ runEval $ tr >>= generateCode
    hPutStr outp result
    hClose inp
    hClose outp
