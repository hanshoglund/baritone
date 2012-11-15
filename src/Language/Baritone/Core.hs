
{-# LANGUAGE StandaloneDeriving #-}

module Language.Baritone.Core where

import Language.Haskell.Syntax
import Data.Semigroup
import Data.List (intersperse)
import Data.List.Split (splitOn)
import Text.Pretty


type BName = String -- unqualified
type BModuleName = [String]

data BImportDecl 
    = BImportDecl 
        BModuleName 
        [BName] 
        (Maybe BName) -- name hiding alias

data BValueDecl = 
    BValueDecl 
        String 
        BExp

data BModule
    = BModule
        BModuleName
        [BImportDecl]
        [BValueDecl]

data BExp
    = BVar BName
    | BApp [BExp]
    | BAbs [BName] BExp
    | BInl String
    | BNum Double
    | BStr String
isBAbs (BAbs _ _) = True
isBAbs _          = False

deriving instance Show BImportDecl
deriving instance Show BValueDecl
deriving instance Show BModule
deriving instance Show BExp

-------------------------------------------------------------------------
-- Haskell to Core
-------------------------------------------------------------------------

-- | Compile a Haskell module into a Baritone module.
toCore :: HsModule -> BModule
toCore (HsModule l n es is as) 
    = BModule 
        (translateModuleName n)
        (map translateImport is)
        (map translateDecl as)
        

    where
        translateModuleName :: Module -> BModuleName
        translateModuleName (Module n) = splitOn "." n

        translateExport :: HsExportSpec -> ()
        translateExport = notSupported
        -- TODO handle export spec (by renaming?)
        
        translateImport :: HsImportDecl -> BImportDecl
        translateImport = notSupported
        -- TODO name resolution/mangling
        
        translateDecl :: HsDecl -> BValueDecl
        translateDecl (HsPatBind l ps a ws)               = translatePatBind ps a ws
        translateDecl (HsFunBind [HsMatch l n [ps] a ws]) = translateFunBind n ps a ws
        translateDecl _                                   = notSupported
        -- TODO multiple bindings

        translatePatBind :: HsPat -> HsRhs -> [HsDecl] -> BValueDecl
        translatePatBind ps a ws = BValueDecl (translatePattern ps) (translateRhs a)
        -- TODO with clause
        
        translateFunBind :: HsName -> HsPat -> HsRhs -> [HsDecl] -> BValueDecl
        translateFunBind n ps a ws = BValueDecl (getHsName n) (translateRhs a)

        translateRhs :: HsRhs -> BExp
        translateRhs (HsUnGuardedRhs a) = translateExpr a
        translateRhs _                  = notSupported
        


        translateExpr :: HsExp -> BExp
        translateExpr (HsParen a)               = translateExpr a

        -- core
        translateExpr (HsVar n)                 = translateQName n
        translateExpr (HsApp f a)               = BApp [translateExpr f, translateExpr a]
        translateExpr (HsNegApp a)              = BApp [BInl "Baritone.negate", translateExpr a]
        translateExpr (HsInfixApp a f b)        = BApp [translateQName (getHsQOp f), translateExpr a, translateExpr b]
        translateExpr (HsLeftSection a f)       = BApp [translateQName (getHsQOp f), translateExpr a]
        translateExpr (HsRightSection f a)      = BApp [translateQName (getHsQOp f), translateExpr a]
        translateExpr (HsLambda l ps as)        = BAbs (map translatePattern ps) (translateExpr as)
        
        -- literals
        translateExpr (HsLit l)                 = translateLiteral l

        -- special
        translateExpr (HsCon n)                 = notSupported
        translateExpr (HsTuple as)              = notSupported
        translateExpr (HsList as)               = notSupported
        translateExpr (HsLet ds a)              = notSupported
        translateExpr (HsIf p a b)              = notSupported
        translateExpr (HsCase p as)             = notSupported
        translateExpr (HsDo as)                 = notSupported

        -- sugar
        translateExpr (HsRecConstr n m)         = notSupported
        translateExpr (HsRecUpdate n m)         = notSupported
        translateExpr (HsEnumFrom a)            = notSupported
        translateExpr (HsEnumFromTo a b)        = notSupported
        translateExpr (HsEnumFromThen a b)      = notSupported
        translateExpr (HsEnumFromThenTo a b c)  = notSupported
        translateExpr (HsListComp a as)         = notSupported

        -- types
        translateExpr (HsExpTypeSig l a t)      = notSupported

        -- patterns
        translateExpr (HsAsPat n a)             = notSupported
        translateExpr (HsWildCard)              = notSupported
        translateExpr (HsIrrPat a)              = notSupported
        
        
        translatePattern :: HsPat -> BName
        translatePattern (HsPVar n) = getHsName n
        translatePattern _          = notSupported
        -- TODO proper matching
        
        translateQName :: HsQName -> BExp
        translateQName (Qual m n)                = notSupported -- TODO
        translateQName (UnQual n)                = BVar (getHsName n)
        translateQName (Special HsUnitCon)       = BInl "null"
        translateQName (Special HsListCon)       = BInl "CreateSparseArray()"
        translateQName (Special (HsTupleCon n))  = notSupported
        translateQName (Special HsFunCon)        = notSupported
        translateQName (Special HsCons)          = notSupported
                
        translateLiteral :: HsLiteral -> BExp
        translateLiteral (HsChar c)     = BStr [c]
        translateLiteral (HsString s)	= BStr s
        translateLiteral (HsInt i)	    = BNum (fromIntegral i)
        translateLiteral (HsFrac r)	    = BNum (fromRational r)
        translateLiteral _              = notSupported
        
        getHsQOp (HsQVarOp n)  = n
        getHsQOp (HsQConOp n)  = n
        getHsName (HsIdent n)  = n
        getHsName (HsSymbol n) = n
                
        notSupported = error "This Haskell feature is not supported"

-------------------------------------------------------------------------
-- Core to ManuScript
-------------------------------------------------------------------------

-- | Compile a Baritone module into a ManuScript plugin.
fromCore :: BModule -> MPlugin
fromCore (BModule n ds vs) = MPlugin 
                               (mangleModuleName n)
                               (map translateValueDecl vs)
    where

        -- TODO resolve imports and rename


        translateValueDecl :: BValueDecl -> MPluginDecl
        translateValueDecl (BValueDecl s a) 
            | isBAbs a      = error "TODO"
            | otherwise     = error "TODO"

        mangleModuleName :: BModuleName -> MName
        mangleModuleName = mconcat . intersperse "_"

        fromCoreExpr :: BExp -> MExp
        -- If n is free in this context, just generate x
        -- otherwise, generate Baritone.lookup(c,x)
        fromCoreExpr (BVar n)       = MVar (MId n)
        --    Generate
        --      f.call(x)
        fromCoreExpr (BApp (f:as))  = MCall (fromCoreExpr f) (map fromCoreExpr as)
        --    Cause generation of method M
        --    Generate code to
        --     * Create a dictionary D
        --     * Copy in free vars of as to D
        --     * Call D.SetMethod(M,'call',as)
        fromCoreExpr (BAbs n a)     = error "TODO"
        fromCoreExpr (BNum a)       = MNum a
        fromCoreExpr (BStr s)       = MStr s
        fromCoreExpr (BInl c)       = MInl c 
                                          
-------------------------------------------------------------------------
-- ManuScript
-------------------------------------------------------------------------

-- http://www.simkin.co.uk/Docs/java/index.html

type MName = String    -- unqualified
type MOpName = String

data MPlugin
    = MPlugin 
        MName 
        [MPluginDecl]
instance Pretty MPlugin where
    pretty (MPlugin n ds)   = string "//" <+> pretty n 
                                </> braces (vcat $ map pretty ds)

data MPluginDecl
    = MGlobal MName MExp            -- name body
    | MMethod MName [MName] [MStm]  -- name vars body
instance Pretty MPluginDecl where
    pretty (MGlobal n a)    = pretty n <+> quotes (pretty a) 
    pretty (MMethod n vs a) = pretty n <+> quotes
                                (parens (sepBy (string ", ") $ map pretty vs)
                                    </> 
                                    sepBy mempty (map pretty a)) -- or just pretty a


data MStm
    = MIf       MExp [MStm] [MStm]
    | MWhile    MExp [MStm]
    | MFor      MName MExp MExp (Maybe MExp) [MStm]  -- var from to step body
    | MForEach  (Maybe MName) MName MExp [MStm]      -- type? var iterable body
    | MSwitch   MExp [(MExp, [MStm])] (Maybe [MStm]) -- disamb cases default
    | MAssign   MVar MExp
    | MReturn   MExp
    | MEmpty
instance Pretty MStm where
    pretty (MIf p a b)          = string "if" <+> parens (pretty p)
                                  <//> string "{" <//> indent 1 (pretty a) <//> string "}"
                                  <//> string "else"
                                  <//> string "{" <//> indent 1 (pretty a) <//> string "}"

    pretty (MWhile p a)         = string "while" <+> parens (pretty p)
                                  <//> string "{" <//> indent 1 (vcat $ map pretty a) <//> string "}"

    pretty (MFor v i j k a)     = string "for" <+> parens clause
                                  <//> string "{" <//> indent 1 (vcat $ map pretty a) <//> string "}"
        where clause            = pretty v 
                                  <+> string "=" <+> pretty i
                                  <+> string "to" <+> pretty j
                                  <+> maybe mempty (\k -> string "step" <+> pretty k) k

    pretty (MForEach t v u a)   = string "for" <+> string "each" <+> clause
                                  <//> string "{" <//> indent 1 (vcat $ map pretty a) <//> string "}"
        where clause            = maybe mempty (\t -> pretty t) t
                                  <+> pretty v
                                  <+> string "in" <+> pretty u

    pretty (MSwitch b cs d)     = error "TODO"

    pretty (MAssign n a)        = (pretty n <+> string "=" <+> pretty a) <> string ";"
    pretty (MReturn a)          = (string "return" <+> pretty a) <> string ";"
    pretty (MEmpty)             = string ";"

data MExp
    = MOp1      MOpName MExp
    | MOp2      MOpName MExp MExp
    | MCall     MExp [MExp]
    | MVar      MVar
    | MInl      String
    | MStr      String
    | MNum      Double
    | MBool     Bool
    | MSelf
    | MNull
instance Pretty MExp where
    pretty (MOp1 n a)   = pretty n <+> pretty a
    pretty (MOp2 n a b) = pretty a <+> string n <+> pretty b
    pretty (MCall n as) = pretty n <> parens (sepBy (string ", ") $ map pretty as)
    pretty (MVar n)     = pretty n
    pretty (MInl c)     = string c
    pretty (MStr s)     = string . show $ s
    pretty (MNum a)     = double a
    pretty (MBool a)    = if a then string "true" else string "false"
    pretty (MSelf)      = string "self"
    pretty (MNull)      = string "null"

data MVar 
    = MId       MName
    | MProp     MVar MName -- ^ @a.n@
    | MPropDef  MVar MName -- ^ @a._property:n@
    | MIndex    MVar MExp  -- ^ @a[n]@
instance Pretty MVar where
    pretty (MId n)        = string n
    pretty (MProp n a)    = pretty n <> string "." <> string a
    pretty (MPropDef n a) = pretty n <> string "._property:" <> string a
    pretty (MIndex n a)   = pretty n <> brackets (pretty a)



indent n = nest (4 * n)


