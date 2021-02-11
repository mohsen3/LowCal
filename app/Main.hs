module Main where

import Text.Megaparsec (parseTest)
import SrcParser

main :: IO ()
main = parseTest functionParser $ unlines [ "def simple(a, y) do"
                                          , "[1, _, a, y]"
                                          , "let x = 11"
                                          , "List.length([x])"
                                          , "sort([1, 3, 4])"
                                          , "if Int.lt(2, a) then 3 else 5 end"
                                          , "end"
                                          ]
