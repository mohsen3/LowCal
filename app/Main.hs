module Main where

import Text.Megaparsec (parseTest)
import SrcParser

main :: IO ()
main = parseTest functionParser (unlines ["def simple(a, y) do", " 1 2", "end"])
