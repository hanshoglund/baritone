
module Language.Baritone.Core where
import Language.Haskell.Syntax

type BName = String -- unqualified
type BModuleName = [String]
type BImportDecl = (BModuleName, [BName], Maybe BName) -- name hiding alias
type BValueDecl = (String, BExpr)

data BModule
    = BModule
        BModuleName
        [BImportDecl]
        [BValueDecl]

data BExpr
    = BVar BName
    | BApp [BExpr]
    | BAbs BName BExpr
    | BInl String        -- inline ManuScript code
    | BNum Double
    | BStr String

    
-------------------------------------------------------------------------
-- Haskell to Core
-------------------------------------------------------------------------

toCore :: HsModule -> BModule
toCore = undefined

fromCore :: BModule -> MPlugin
fromCore = undefined

-------------------------------------------------------------------------
-- Core to ManuScript
-------------------------------------------------------------------------

type MName = String -- unqualified

-- http://www.simkin.co.uk/Docs/java/index.html

data MPlugin
    = MPlugin 
        MName 
        [MPluginDecl]

data MPluginDecl
    = MGlobal
        MName
        MExpr
    | MMethod 
        MName   -- name of method
        [MName] -- vars
        MExpr

type MQName = [MName]
type MOpName = String

data MStm
    = MIf       MExpr [MStm] [MStm]
    | MWhile    MExpr [MStm]
    | MFor      MName MExpr MExpr (Maybe MExpr) [MStm] -- var from to step body
    | MForEach  (Maybe MName) MName MExpr [MStm]       -- type? var iterable body
    | MSwitch   MExpr [(MExpr, [MStm])] (Maybe [MStm]) -- disamb cases default
    | MAssign   MVar MExpr
    | MReturn   MExpr
    | MEmpty

data MExpr
    = MOp1      MOpName MExpr
    | MOp2      MOpName MExpr MExpr
    | MCall     MExpr [MExpr]
    | MVar      MVar
    | MStr      String
    | MNum      Double
    | MBool     Bool
    | MSelf
    | MNull

data MVar 
    = MId       MQName
    | MProp     MQName MExpr
    | MIndex    MQName MExpr


