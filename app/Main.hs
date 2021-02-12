module Main where

import Control.Monad (when)
import Text.Megaparsec (parse, parseTest)
import Text.Pretty.Simple (pPrint)
import System.Environment (getArgs)

import SrcParser
import TranspileJS

main :: IO ()
main = do
    args <- getArgs
    when (length args /= 1) (error "Usage: lowcalc filepath.lowcal")

    let fileName = head args
    src <- readFile fileName
    case parse moduleParser fileName src of
        Left err -> print $ err
        Right mod -> do
            pPrint mod
            putStrLn "******* JS ********"
            putStr $ transpileModule mod
