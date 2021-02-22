{-# LANGUAGE QuasiQuotes #-}

module TranspileJS where

import Data.List (intercalate)
import Data.String.Interpolate ( i )

import SrcTypes


transpileModule :: SrcModule -> String
transpileModule (SrcModule funcDefs) =
    let
        js = unlines $ fmap transpileFunction funcDefs
        executable = any (\(SrcFunctionDef fname _ _) -> (fname == "main")) funcDefs
    in 
        if executable then js ++ "\nmain();" else js

transpileFunction :: SrcFunctionDef -> String
transpileFunction (SrcFunctionDef name args body) =
    [i|function #{name}(#{intercalate ", " args}) { #{transpileBlock body} }|]

transpileExp :: SrcExp -> String
transpileExp (SrcExpPrimitive prim) = transpilePrimitive prim
transpileExp (SrcExpVar varName) = varName
transpileExp (SrcExpList vals) = concat ["[", intercalate ", " $ fmap transpileExp vals, "]"]
transpileExp (SrcLetExp lhs rhs) = concat ["var ", lhs, " = ", transpileExp rhs]
transpileExp (SrcIfExp cond thenBlock elseBlock) =
    [i|(function(){
         if (
             #{transpileExp cond}
            ) {
            #{transpileBlock thenBlock}
         } else {
            #{transpileBlock elseBlock}
         }
      })()
    |]

transpileExp (SrcFunctionCall maybeModuleName funcName realArgs) =
    if holeCount > 0
    then
        let fnHoleParams = intercalate ", " ["hole_" ++ show i | i <- [1..holeCount]]
        in  [i|function(#{fnHoleParams}) { return (#{funcCall}); }|]
    else
        funcCall
    where
        holeCount = length $ filter (SrcExpPrimitive SrcHole ==) realArgs
        
        replaceHoles [] _ = []
        replaceHoles (SrcExpPrimitive SrcHole:args) (i:is) = SrcExpVar ("hole_" ++ show i):replaceHoles args is
        replaceHoles (x:args) is = x:replaceHoles args is

        args = replaceHoles realArgs [1..holeCount]

        funcCall = case maybeModuleName of
            Just moduleName -> nativeFuncCall moduleName funcName
            Nothing -> concat [funcName, "(", intercalate ", " $ fmap transpileExp args ,")"]

        nativeFuncCall "IO" "print" = concat ["console.log(", transpileExp (head args), ")"]
        nativeFuncCall "List" "append" = concat ["(", transpileExp (head args), ").concat(", transpileExp (last args), ")"]
        nativeFuncCall "List" "filter" = concat ["(", transpileExp (head args), ").filter(", transpileExp (last args), ")"]
        nativeFuncCall "List" "get" = concat ["(", transpileExp (head args), ")[", transpileExp (last args), "]"]
        nativeFuncCall "List" "length" = concat ["(", transpileExp (head args), ").length"]
        nativeFuncCall "List" "sublist" = concat ["(", transpileExp (head args), ").slice(", transpileExp (args !! 1), ", ", transpileExp (args !! 2), ")"]
        nativeFuncCall "Int" "eq" = opstr "==="
        nativeFuncCall "Int" "gt" = opstr ">"
        nativeFuncCall "Int" "gte" = opstr ">="
        nativeFuncCall "Int" "lt" = opstr "<"
        nativeFuncCall "Int" "lte" = opstr "<="
        nativeFuncCall "Int" "sub" = opstr "-"
        nativeFuncCall "Int" "add" = opstr "+"
        nativeFuncCall "Int" "pow" = opstr "**"
        nativeFuncCall moduleName funcName = error $ "Unknown native function: " ++ moduleName ++ "." ++ funcName

        opstr op  = concat [ "(", transpileExp (head args), ") ", op, " (", transpileExp (last args), ")"]

transpileBlock :: [SrcExp] -> String
transpileBlock [x] = concat ["return (", transpileExp x, ");"]
transpileBlock (h:r) = concat [transpileExp h, ";\n", transpileBlock r]

transpilePrimitive :: SrcPrimitive -> String
transpilePrimitive (SrcIntValue v) = show v
transpilePrimitive (SrcBoolean True) = "true"
transpilePrimitive (SrcBoolean False) = "false"
transpilePrimitive (SrcString v) = show v
transpilePrimitive (SrcHole) = error "Cannot transpile hole!"
