module SrcTypes where

data SrcModule = SrcModule [SrcFunctionDef] deriving (Eq, Show)

data SrcFunctionDef = SrcFunctionDef String [String] [SrcExp] deriving (Eq, Show)

data SrcExp =
    SrcIfExp SrcExp [SrcExp] [SrcExp] |
    SrcLetExp String SrcExp |
    SrcFunctionCall (Maybe String) String [SrcExp] |
    SrcExpPrimitive SrcPrimitive |
    SrcExpList [SrcExp] |
    SrcExpVar String deriving (Eq, Show)

data SrcPrimitive =
    SrcIntValue Integer |
    SrcBoolean Bool |
    SrcString String |
    SrcHole deriving (Eq, Show)

