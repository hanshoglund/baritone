
module Language.Baritone.Core where
import Language.Haskell.Syntax
import Data.Semigroup
import Text.Pretty

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

-- http://www.simkin.co.uk/Docs/java/index.html

type MName = String -- unqualified

data MPlugin
    = MPlugin 
        MName 
        [MPluginDecl]
instance Pretty MPlugin where
    pretty = error "Missing"

data MPluginDecl
    = MGlobal MName MExpr           -- name body
    | MMethod MName [MName] MExpr   -- name vars body
instance Pretty MPluginDecl where
    pretty = error "Missing"

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
instance Pretty MStm where
    pretty (MIf p a b)          = string "if" <+> parens (pretty p)
                                  <//> string "{" <//> nest 1 (pretty a) <//> string "}"
                                  <//> string "else"
                                  <//> string "{" <//> nest 1 (pretty a) <//> string "}"
    pretty (MWhile p a)         = string "while" <+> parens (pretty p)
                                  <//> string "{" <//> nest 1 (pretty a) <//> string "}"
    pretty (MFor v i j k a)     = string "for" <+> parens clause
                                  <//> string "{" <//> nest 1 (pretty a) <//> string "}"
        where
            clause = pretty v 
                <+> string "=" <+> pretty i
                <+> string "to" <+> pretty j
                <+> maybe mempty (\k -> string "step" <+> pretty k) k
    pretty (MForEach t v u a)   = string "for" <+> string "each" <+> parens clause
                                  <//> string "{" <//> nest 1 (pretty a) <//> string "}"
        where
            clause = pretty v 
                <+> maybe mempty (\t -> pretty t) t
                <+> pretty v
                <+> string "in" <+> pretty u
    pretty (MSwitch b cs d)     = error "TODO"
    pretty (MAssign n a)        = (pretty n <+> string "=" <+> pretty a) <> string ";"
    pretty (MReturn a)          = (string "return" <+> pretty a) <> string ";"
    pretty (MEmpty)             = string ";"

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
instance Pretty MExpr where
    pretty (MOp1 n a)   = pretty n <+> pretty a
    pretty (MOp2 n a b) = pretty a <+> pretty n <+> pretty b
    pretty (MCall n as) = pretty n <> brackets (sepBy (string ", ") $ map pretty as)
    pretty (MVar n)     = pretty n
    pretty (MStr s)     = string . show $ s
    pretty (MNum a)     = double a
    pretty (MBool a)    = if a then string "true" else string "false"
    pretty (MSelf)      = string "self"
    pretty (MNull)      = string "null"

data MVar 
    = MId       MQName
    | MProp     MVar MExpr
    | MIndex    MVar MExpr
instance Pretty MVar where
    pretty (MId n)      = pretty n
    pretty (MProp n a)  = pretty n <> string "." <> pretty a
    pretty (MIndex n a) = pretty n <> brackets (pretty a)


