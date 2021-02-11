module SrcTypes where

data SrcModule = SrcModule { srcFunctionDefs :: [SrcFunctionDef] } deriving (Eq, Show)

data SrcFunctionDef = SrcFunctionDef 
  { srcFunctionName :: String
  , srcFunctionArgs :: [String]
  , srcFunctioBody :: [SrcExp]
  } deriving (Eq, Show)

data SrcExp =
    SrcIfExp { srcIfCondition :: SrcExp, srcThenBlock :: [SrcExp], srcElseBlock :: [SrcExp] } |
    SrcLetExp { srcLetVarName :: String, srcLetValue :: SrcExp } |
    SrcFunctionCall { srcFunctionCallModule :: Maybe String, srcFunctionCallName :: String, srcFunctionCallArgs :: [SrcExp] } |
    SrcExpSimpleValue { srcExpValue :: SrcValue } |
    SrcExpList { srcExpListValues :: [SrcExp] } deriving (Eq, Show)

data SrcValue =
    SrcIntValue Integer |
    SrcBoolean Bool |
    SrcHole deriving (Eq, Show)

