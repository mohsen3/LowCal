module TranspileJS where

import Data.List (intercalate)
import SrcTypes

transpileModule :: SrcModule -> String
transpileModule (SrcModule funcDefs) =
    unlines $ fmap transpileFunction funcDefs

transpileFunction :: SrcFunctionDef -> String
transpileFunction (SrcFunctionDef name args body) =
    concat [ "function "
           , name
           , "("
           , intercalate ", " args
           , ")"
           , "{"
           , intercalate ";\n" $ fmap transpileExp body
           , "}"
           ]

transpileExp :: SrcExp -> String
transpileExp (SrcExpPrimitive prim) = transpilePrimitive prim
transpileExp (SrcExpVar varName) = varName
transpileExp (SrcExpList vals) = concat ["[", intercalate ", " $ fmap transpileExp vals, "]"]
transpileExp (SrcLetExp lhs rhs) = concat ["var ", lhs, " = ", transpileExp rhs]
transpileExp (SrcIfExp cond thenBlock elseBlock) =
    intercalate "\n" [ "(function(){"
                     , "if ("
                     , transpileExp cond
                     , ") {"
                     , transpileBlock thenBlock
                     , "} else {"
                     , transpileBlock elseBlock
                     , "}"
                     , "})()"
                     ]
transpileExp (SrcFunctionCall (Just moduleName) funcName args) =
    case (moduleName, funcName) of
        ("IO", "print") -> concat ["console.log(", transpileExp (head args), ")"]
        ("List", "append") -> concat ["(", transpileExp (head args), ").concat(", transpileExp (last args), ")"]
        ("List", "filter") -> concat ["(", transpileExp (head args), ").filter(", transpileExp (last args), ")"]
        ("List", "get") -> concat ["(", transpileExp (head args), ")[", transpileExp (last args), "]"]
        ("List", "length") -> concat ["(", transpileExp (head args), ").length"]
        ("List", "sublist") -> concat ["(", transpileExp (head args), ").slice(", transpileExp (args !! 1), ", ", transpileExp (args !! 2), ")"]
        ("Int", "lte") -> opstr "<="
        ("Int", "lt") -> opstr "<"
        ("Int", "gte") -> opstr ">="
        ("Int", "sub") -> opstr "-"
        (_, _) -> error $ "Unknown native function: " ++ moduleName ++ "." ++ funcName
    where
        opstr op  = concat [ "(", transpileExp (head args), ") ", op, " (", transpileExp (last args), ")"]

transpileExp (SrcFunctionCall Nothing funcName args) = concat [funcName, "(", intercalate ", " $ fmap transpileExp args ,")"]

transpileBlock :: [SrcExp] -> String
transpileBlock [x] = concat ["return (", transpileExp x, ");"]
transpileBlock (h:r) = concat [transpileExp h, ";\n", transpileBlock r]

transpilePrimitive :: SrcPrimitive -> String
transpilePrimitive (SrcIntValue v) = show v
transpilePrimitive (SrcBoolean True) = "true"
transpilePrimitive (SrcBoolean False) = "false"
transpilePrimitive (SrcHole) = "____" -- error "Cannot transpile hole!"