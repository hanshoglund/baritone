
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
        BExpr

data BModule
    = BModule
        BModuleName
        [BImportDecl]
        [BValueDecl]

data BExpr
    = BVar BName
    | BApp [BExpr]
    | BAbs [BName] BExpr
    | BInl String
    | BNum Double
    | BStr String
isBAbs (BAbs _ _) = True
isBAbs _          = False

-------------------------------------------------------------------------
-- Haskell to Core
-------------------------------------------------------------------------

toCore :: HsModule -> BModule
toCore (HsModule l n es is as) = undefined

    where
        translateName :: Module -> BModuleName
        translateName (Module n) = splitOn "." n
        -- TODO handle unicode?

        translateExport :: HsExportSpec -> ()
        translateExport = error "Not impl"
        -- TODO handle export spec (by renaming?)
        
        translateImport :: HsImportSpec -> ()
        translateImport = error "Not impl"
        -- TODO name resolution/mangling
        
        translateDecl :: HsDecl -> BValueDecl
        translateDecl (HsTypeDecl l n vs t)             = notSupported
        translateDecl (HsDataDecl l c n vs cs ns)       = notSupported
        translateDecl (HsNewTypeDecl l c n vs cs ns)    = notSupported
        translateDecl (HsInfixDecl l a f o)             = notSupported
        translateDecl (HsClassDecl l c n ns ds)         = notSupported
        translateDecl (HsInstDecl l c t ts ds)          = notSupported
        translateDecl (HsDefaultDecl l ts)              = notSupported
        translateDecl (HsForeignImport s h f m n t)     = notSupported
        translateDecl (HsForeignExport s h m n t)       = notSupported
        translateDecl (HsTypeSig l ns t)                = notSupported
        translateDecl (HsPatBind l p a w)               = notSupported
        translateDecl (HsFunBind as)                    = translateMatches as

        translateMatches :: [HsMatch] -> BValueDecl
        translateMatches = undefined
        -- TODO special handling of 'inline'

        
        -- translateDecl
        
        translateExpr :: HsExp -> BExpr
        translateExpr (HsVar n)                 = BVar undefined
        translateExpr (HsInfixApp a f b)        = BApp [undefined, undefined, undefined]
        translateExpr (HsApp f a)               = BApp [undefined, undefined]
        translateExpr (HsNegApp a)              = notSupported -- TODO
        translateExpr (HsCon n)                 = notSupported
        translateExpr (HsLit l)                 = notSupported -- TODO
        translateExpr (HsLambda l p a)          = BAbs [undefined] undefined
        translateExpr (HsLet ds a)              = notSupported
        translateExpr (HsIf p a b)              = notSupported
        translateExpr (HsCase p as)             = notSupported
        translateExpr (HsDo as)                 = notSupported
        translateExpr (HsTuple as)              = notSupported
        translateExpr (HsList as)               = notSupported
        translateExpr (HsParen a)               = notSupported
        translateExpr (HsLeftSection a f)       = BApp [undefined, undefined]
        translateExpr (HsRightSection f a)      = BApp [undefined, undefined]
        translateExpr (HsRecConstr n m)         = notSupported
        translateExpr (HsRecUpdate n m)         = notSupported
        translateExpr (HsEnumFrom a)            = notSupported
        translateExpr (HsEnumFromTo a b)        = notSupported
        translateExpr (HsEnumFromThen a b)      = notSupported
        translateExpr (HsEnumFromThenTo a b c)  = notSupported
        translateExpr (HsListComp a as)         = notSupported
        translateExpr (HsExpTypeSig l a t)      = notSupported
        translateExpr (HsAsPat n a)             = notSupported
        translateExpr (HsWildCard)              = notSupported
        translateExpr (HsIrrPat a)              = notSupported


                
        notSupported = error "This Haskell feature is not supported"

-------------------------------------------------------------------------
-- Core to ManuScript
-------------------------------------------------------------------------

-- TODO resolve imports and rename
fromCore :: BModule -> MPlugin
fromCore (BModule n ds vs) = MPlugin 
                               (mangleModuleName n)
                               (map translateValueDecl vs)
    where

        translateValueDecl :: BValueDecl -> MPluginDecl
        translateValueDecl (BValueDecl s a) 
            | isBAbs a      = error "TODO"
            | otherwise     = error "TODO"

        mangleModuleName :: BModuleName -> MName
        mangleModuleName = mconcat . intersperse "_"

        fromCoreExpr :: BExpr -> MExpr
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
    = MGlobal MName MExpr           -- name body
    | MMethod MName [MName] [MStm]  -- name vars body
instance Pretty MPluginDecl where
    pretty (MGlobal n a)    = pretty n <+> quotes (pretty a) 
    pretty (MMethod n vs a) = pretty n <+> quotes
                                (parens (sepBy (string ", ") $ map pretty vs)
                                    </> 
                                    sepBy mempty (map pretty a)) -- or just pretty a


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

data MExpr
    = MOp1      MOpName MExpr
    | MOp2      MOpName MExpr MExpr
    | MCall     MExpr [MExpr]
    | MVar      MVar
    | MInl      String
    | MStr      String
    | MNum      Double
    | MBool     Bool
    | MSelf
    | MNull
instance Pretty MExpr where
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
    | MIndex    MVar MExpr -- ^ @a[n]@
instance Pretty MVar where
    pretty (MId n)        = string n
    pretty (MProp n a)    = pretty n <> string "." <> string a
    pretty (MPropDef n a) = pretty n <> string "._property:" <> string a
    pretty (MIndex n a)   = pretty n <> brackets (pretty a)



indent n = nest (4 * n)


