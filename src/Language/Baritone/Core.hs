
{-# LANGUAGE StandaloneDeriving #-}

module Language.Baritone.Core where

import Language.Haskell.Syntax
import Control.Monad.Writer hiding ((<>))
import Control.Monad.State
import Data.Semigroup
import Data.List (intersperse, partition)
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
    | BApp BExp [BExp]
    | BAbs [BName] BExp
    | BInl String
    | BNum Double
    | BStr String
isBApp (BApp _ _) = True
isBApp _          = False
isBAbs (BAbs _ _) = True
isBAbs _          = False

deriving instance Show BImportDecl
deriving instance Show BValueDecl
deriving instance Show BModule
deriving instance Show BExp

instance Pretty BImportDecl where
    pretty (BImportDecl n hs a) = string "import"
                                    <+> string "hiding" <+> parens (sepBy (string ",") $ map pretty hs)
                                    <+> string "as" <+> maybe mempty string a
instance Pretty BValueDecl where
    pretty (BValueDecl n a)     = string n </> nest 12 (string "=" <+> pretty a)
instance Pretty BModule where
    pretty (BModule n is as)    = string "module"
                                    <+> string (concatWith "." n)
                                    <+> vcat (map pretty is)
                                    <+> string "where"
                                    <//> empty
                                    <//> vcat (map pretty as)
instance Pretty BExp where
    pretty (BVar n)     = string n
    pretty (BApp f as)  = hsep (map pretty' $ f:as)
    pretty (BAbs ns a)  = (string "\\" <-> hsep (map string ns) <+> string "->")
                            <+> pretty a
    pretty (BInl s)     = string "inline" <+> string s
    pretty (BNum n)     = string (show n)
    pretty (BStr s)     = string (show s)

pretty' x | isBApp x  = parens (pretty x)
          | otherwise = pretty x
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
        translateDecl (HsPatBind l p a ws)              = translatePatBind p a ws
        translateDecl (HsFunBind [HsMatch l n ps a ws]) = translateFunBind n ps a ws
        translateDecl _                                 = notSupported
        -- TODO multiple match clauses

        translatePatBind :: HsPat -> HsRhs -> [HsDecl] -> BValueDecl
        translatePatBind p a ws = BValueDecl (translatePattern p) (translateRhs a)
        -- TODO with clause

        translateFunBind :: HsName -> [HsPat] -> HsRhs -> [HsDecl] -> BValueDecl
        translateFunBind n ps a ws = BValueDecl (getHsName n) (BAbs (map translatePattern ps) (translateRhs a))

        translateRhs :: HsRhs -> BExp
        translateRhs (HsUnGuardedRhs a) = translateExpr a
        translateRhs _                  = notSupported



        translateExpr :: HsExp -> BExp
        translateExpr (HsParen a)               = translateExpr a

        -- core
        translateExpr (HsVar n)                 = translateQName n
        translateExpr (HsApp f a)               = BApp (translateExpr f) [translateExpr a]
        translateExpr (HsNegApp a)              = BApp (BInl "(-)") [translateExpr a]
        translateExpr (HsInfixApp a f b)        = BApp (wrapOp . translateQName . getHsQOp $ f) [translateExpr a, translateExpr b]
        translateExpr (HsLeftSection a f)       = BApp (wrapOp . translateQName . getHsQOp $ f) [translateExpr a]
        translateExpr (HsRightSection f a)      = BApp (wrapOp . translateQName . getHsQOp $ f) [translateExpr a]
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

        wrapOp (BVar x) = BVar $ "(" ++ x ++ ")"
        wrapOp x        = x

        notSupported = error "This Haskell feature is not supported"

-------------------------------------------------------------------------
-- Core to ManuScript
-------------------------------------------------------------------------

-- | Compile a Baritone module into a ManuScript plugin.
fromCore :: BModule -> MPlugin
fromCore (BModule n is as) = MPlugin
                               (translateModuleName n)
                               (foldGlobals . execMGen $ mapM translateValueDecl as >> return ())

    where               
        -- rewrite globals as self._property assignment
        foldGlobals :: [MPluginDecl] -> [MPluginDecl]
        foldGlobals xs = handleGlobals gs ++ ms
            where
                (gs, ms) = partition isMGlobal xs
                
                handleGlobals :: [MPluginDecl] -> [MPluginDecl]
                handleGlobals x = [MMethod "Initialize" [] (mconcat $ map toSelfAssign x)]
                
                toSelfAssign :: MPluginDecl -> [MStm]
                toSelfAssign (MGlobal n a) = [MAssign (MPropDef (MId "Self") n) a]
        
        translateModuleName :: BModuleName -> MName
        translateModuleName = concatWith "_"

        translateValueDecl :: BValueDecl -> MGen ()
        translateValueDecl (BValueDecl s a) = do
            a' <- fromCoreExpr a
            addGlobal s a'
            return ()
            
        fromCoreExpr :: BExp -> MGen MExp
        fromCoreExpr (BVar n)      = do
            return $ MVar (MId n)        
        
        fromCoreExpr (BApp f as) = do
            f'  <- fromCoreExpr . fixPrimOps $ f
            as' <- mapM fromCoreExpr as
            return $ MCall f' as'

        fromCoreExpr (BAbs ns a)     = do
            m <- addUniqueMethod [] [MExp $ MCall (MVar $ MId "trace") [MStr "Called method!"]] -- TODO
            return $ MCall (MVar $ MId m) []

        fromCoreExpr (BNum a) = return $ MNum a
        fromCoreExpr (BStr s) = return $ MStr s
        fromCoreExpr (BInl c) = return $ MInl c

        fixPrimOps :: BExp -> BExp
        fixPrimOps (BVar f) = (BVar $ primOp f)
        fixPrimOps x        = x
        
        primOp :: BName -> BName
        primOp "(+)" = "B.add"
        primOp "(-)" = "B.sub"
        primOp "(*)" = "B.mul"
        primOp "(/)" = "B.div"
        primOp x     = x

-------------------------------------------------------------------------
-- ManuScript
-------------------------------------------------------------------------

-- http://www.simkin.co.uk/Docs/java/index.html

-- |
-- Plugin generation monad including:
--  * A state for counting the number of generated functions
--  * A writer for collecting the generated functions
type MGen = WriterT [MPluginDecl] (State Int)

addGlobal :: MName -> MExp -> MGen ()
addGlobal n e = do
    tell [MGlobal n e]
    return ()

addMethod :: MName -> [MName] -> [MStm] -> MGen ()
addMethod n vs as = do
    tell [MMethod n vs as]
    return ()

addUniqueMethod :: [MName] -> [MStm] -> MGen MName
addUniqueMethod vs as = do
    c <- get
    put (succ c)   
    let n = "__" ++ show c
    tell [MMethod n vs as]
    return n

execMGen :: MGen () -> [MPluginDecl]
execMGen x = evalState (execWriterT x) 0

type MName = String    -- unqualified
type MOpName = String

data MPlugin
    = MPlugin
        MName
        [MPluginDecl]
    deriving (Show, Eq)
instance Pretty MPlugin where
    pretty (MPlugin n ds)   = string "//" <+> pretty n
                                </> braces (vcat $ map pretty ds)

data MPluginDecl
    = MGlobal MName MExp            -- name body
    | MMethod MName [MName] [MStm]  -- name vars body
    deriving (Show, Eq)
isMGlobal (MGlobal _ _) = True
isMGlobal _             = False

instance Pretty MPluginDecl where
    pretty (MGlobal n a)    = string n <+> doubleQuotes (asStr a)
        where
            asStr (MStr s) = string s
            asStr a        = pretty a
    pretty (MMethod n vs a) = string n <+> doubleQuotes
                                (parens (sepBy (string ", ") $ map string vs)
                                    <//>
                                 (braces . indent 1) (mempty
                                    <//> vcat (map pretty a))
                                    <//> mempty)


data MStm
    = MIf       MExp [MStm] [MStm]
    | MWhile    MExp [MStm]
    | MFor      MName MExp MExp (Maybe MExp) [MStm]  -- var from to step body
    | MForEach  (Maybe MName) MName MExp [MStm]      -- type? var iterable body
    | MSwitch   MExp [(MExp, [MStm])] (Maybe [MStm]) -- disamb cases default
    | MAssign   MVar MExp
    | MReturn   MExp
    | MExp      MExp
    | MEmpty
    deriving (Show, Eq)
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

    pretty (MAssign n a)        = (pretty n <+> indent 5 (string "=" <+> pretty a) <-> string ";")
    pretty (MReturn a)          = (string "return" <+> pretty a) <> string ";"
    pretty (MExp a)             = (pretty a) <> string ";"
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
    deriving (Show, Eq)
instance Pretty MExp where
    pretty (MOp1 n a)   = pretty n <+> pretty a
    pretty (MOp2 n a b) = pretty a <+> string n <+> pretty b
    pretty (MCall n as) = pretty n <> parens (sepBy (string ", ") $ map pretty as)
    pretty (MVar n)     = pretty n
    pretty (MInl c)     = string c
    pretty (MStr s)     = quotes (string $ s) -- TODO proper escaping
    pretty (MNum a)     = double a
    pretty (MBool a)    = if a then string "true" else string "false"
    pretty (MSelf)      = string "self"
    pretty (MNull)      = string "null"

data MVar
    = MId       MName
    | MProp     MVar MName -- ^ @a.n@
    | MPropDef  MVar MName -- ^ @a._property:n@
    | MIndex    MVar MExp  -- ^ @a[n]@
    deriving (Show, Eq)
instance Pretty MVar where
    pretty (MId n)        = string n
    pretty (MProp n a)    = pretty n <> string "." <> string a
    pretty (MPropDef n a) = pretty n <> string "._property:" <> string a
    pretty (MIndex n a)   = pretty n <> brackets (pretty a)



indent n = nest (4 * n)
spaces x = mempty <+> x <+> mempty x
concatWith x = mconcat . intersperse x

