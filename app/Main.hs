module Main where

import Text.Megaparsec (parseTest)
import SrcParser

main :: IO ()
main = do
    src <- readFile "sort.lowcal"
    parseTest functionParser src
