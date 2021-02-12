module Main where

import Text.Megaparsec (parse, parseTest)
import SrcParser
import TranspileJS

main :: IO ()
main = do
    let fileName = "sort.lowcal"
    src <- readFile fileName
    case parse moduleParser fileName src of
        Left err -> print $ err
        Right mod -> putStr $ transpileModule mod
