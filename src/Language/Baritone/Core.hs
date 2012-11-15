
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

data BImp
    = BImp
        BModuleName
        [BName]
        (Maybe BName) -- name hiding alias

data BValue =
    BValue
        String
        BExp

data BModule
    = BModule
        BModuleName
        [BImp]
        [BValue]

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

deriving instance Show BImp
deriving instance Show BValue
deriving instance Show BModule
deriving instance Show BExp

instance Pretty BImp where
    pretty (BImp n hs a) = string "import"
                                    <+> string "hiding" <+> parens (sepBy (string ",") $ map pretty hs)
                                    <+> string "as" <+> maybe mempty string a
instance Pretty BValue where
    pretty (BValue n a)     = string n </> nest 12 (string "=" <+> pretty a)
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
        (transModName n)
        (map transImSpec is)
        (map transDec as)

    where

        transModName :: Module -> BModuleName
        transModName (Module n) = splitOn "." n

        transExSpec :: HsExportSpec -> ()
        transExSpec = notSupported
        -- TODO handle export spec (by renaming?)

        transImSpec :: HsImportDecl -> BImp
        transImSpec = notSupported
        -- TODO name resolution/mangling

        transDec :: HsDecl -> BValue
        transDec (HsPatBind l p a ws)              = transPatBind p a ws
        transDec (HsFunBind [HsMatch l n ps a ws]) = translateFunBind n ps a ws
        transDec _                                 = notSupported
        -- TODO multiple match clauses

        transPatBind :: HsPat -> HsRhs -> [HsDecl] -> BValue
        transPatBind p a ws = BValue (transPat p) (transRhs a)
        -- TODO with clause

        translateFunBind :: HsName -> [HsPat] -> HsRhs -> [HsDecl] -> BValue
        translateFunBind n ps a ws = BValue (getHsName n) (BAbs (map transPat ps) (transRhs a))

        transRhs :: HsRhs -> BExp
        transRhs (HsUnGuardedRhs a) = transExp a
        transRhs _                  = notSupported



        transExp :: HsExp -> BExp
        transExp (HsParen a)               = transExp a

        -- core
        transExp (HsVar n)                 = transQName n
        transExp (HsApp f a)               = BApp (transExp f) [transExp a]
        transExp (HsNegApp a)              = BApp (BInl "(-)") [transExp a]
        transExp (HsInfixApp a f b)        = BApp (wrapOp . transQName . getHsQOp $ f) [transExp a, transExp b]
        transExp (HsLeftSection a f)       = BApp (wrapOp . transQName . getHsQOp $ f) [transExp a]
        transExp (HsRightSection f a)      = BApp (wrapOp . transQName . getHsQOp $ f) [transExp a]
        transExp (HsLambda l ps as)        = BAbs (map transPat ps) (transExp as)

        -- literals
        transExp (HsLit l)                 = transLit l

        -- special
        transExp (HsCon n)                 = notSupported
        transExp (HsTuple as)              = notSupported
        transExp (HsList as)               = notSupported
        transExp (HsLet ds a)              = notSupported
        transExp (HsIf p a b)              = notSupported
        transExp (HsCase p as)             = notSupported
        transExp (HsDo as)                 = notSupported

        -- sugar
        transExp (HsRecConstr n m)         = notSupported
        transExp (HsRecUpdate n m)         = notSupported
        transExp (HsEnumFrom a)            = notSupported
        transExp (HsEnumFromTo a b)        = notSupported
        transExp (HsEnumFromThen a b)      = notSupported
        transExp (HsEnumFromThenTo a b c)  = notSupported
        transExp (HsListComp a as)         = notSupported

        -- types
        transExp (HsExpTypeSig l a t)      = notSupported

        -- patterns
        transExp (HsAsPat n a)             = notSupported
        transExp (HsWildCard)              = notSupported
        transExp (HsIrrPat a)              = notSupported


        transPat :: HsPat -> BName
        transPat (HsPVar n) = getHsName n
        transPat _          = notSupported
        -- TODO proper matching

        transQName :: HsQName -> BExp
        transQName (Qual m n)                = notSupported -- TODO
        transQName (UnQual n)                = BVar (getHsName n)
        transQName (Special HsUnitCon)       = BInl "null"
        transQName (Special HsListCon)       = BInl "CreateSparseArray()"
        transQName (Special (HsTupleCon n))  = notSupported
        transQName (Special HsFunCon)        = notSupported
        transQName (Special HsCons)          = notSupported

        transLit :: HsLiteral -> BExp
        transLit (HsChar c)     = BStr [c]
        transLit (HsString s)	= BStr s
        transLit (HsInt i)	    = BNum (fromIntegral i)
        transLit (HsFrac r)	    = BNum (fromRational r)
        transLit _              = notSupported

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
fromCore (BModule n is as)
    = MPlugin
        (transModName n)
        (foldGlobals . execMGen $ mapM transValueDecl as >> return ())

    where

        -- | Rewrite globals as self._property assignment
        foldGlobals :: [MDecl] -> [MDecl]
        foldGlobals xs = handleGlobals gs ++ ms
            where
                (gs, ms) = partition isMGlobal xs

                handleGlobals :: [MDecl] -> [MDecl]
                handleGlobals x = [MMethod "Initialize" [] (mconcat $ map toSelfAssign x)]

                toSelfAssign :: MDecl -> [MStm]
                toSelfAssign (MGlobal n a) = [MAssign (MPropDef MSelf n) a]

        transModName :: BModuleName -> MName
        transModName = concatWith "_"

        transValueDecl :: BValue -> MGen ()
        transValueDecl (BValue s a) = do
            a' <- transExp a
            addGlobal s a'
            return ()



        transExp :: BExp -> MGen MExp
        transExp (BVar n)      = do
            return $ MVar (MId n)

        transExp (BApp f as) = do
            f'  <- transExp . fixPrimOps $ f
            as' <- mapM transExp as
            return $ MCall (MVar $ MProp f' "_a") as'

        transExp (BAbs ns a)     = do
            a' <- transExp a
            invoke <- let         
                vars = ["_c"] ++ ns
                body = [MReturn a']
                in addUniqueMethod vars body
            create <- let
                vars = ["_c"]
                body = [
                    MAssign (MId "_k") (MCall (MVar $ MId "CreateDictionary") []),
                    -- TODO copy free vars
                    MExp (MCall (MVar $ MProp (MVar $ MId "_k") "SetMethod") [MStr "_a", MSelf, MVar (MId invoke)]),
                    MReturn (MVar $ MId "_k")
                    ]
                in addUniqueMethod vars body
            return $ MCall (MVar $ MId create) []

        transExp (BNum a) = return $ MNum a
        transExp (BStr s) = return $ MStr s
        transExp (BInl c) = return $ MInl c




        fixPrimOps :: BExp -> BExp
        fixPrimOps (BVar f) = (BVar $ primOp f)
        fixPrimOps x        = x

        primOp :: BName -> BName
        primOp "(+)" = "_add"
        primOp "(-)" = "_sub"
        primOp "(*)" = "_mul"
        primOp "(/)" = "_div"
        primOp x     = x

-------------------------------------------------------------------------
-- ManuScript
-------------------------------------------------------------------------

-- http://www.simkin.co.uk/Docs/java/index.html

type MName = String    -- unqualified
type MOpName = String

data MPlugin
    = MPlugin
        MName
        [MDecl]
    deriving (Show, Eq)

data MDecl
    = MGlobal MName MExp            -- name body
    | MMethod MName [MName] [MStm]  -- name vars body
    deriving (Show, Eq)

isMGlobal (MGlobal _ _) = True
isMGlobal _             = False

data MStm
    = MIf       MExp [MStm] [MStm]
    | MWhile    MExp [MStm]
    | MFor      MName MExp MExp (Maybe MExp) [MStm]  -- var from to step body
    | MForEach  (Maybe MName) MName MExp [MStm]      -- type? var iterable body
    -- | MSwitch   MExp [(MExp, [MStm])] (Maybe [MStm]) -- disamb cases default
    | MAssign   MVar MExp
    | MReturn   MExp
    | MExp      MExp
    | MEmpty
    deriving (Show, Eq)

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

data MVar
    = MId       MName
    | MProp     MExp MName -- ^ @a.n@
    | MPropDef  MExp MName -- ^ @a._property:n@
    | MIndex    MExp MExp  -- ^ @a[n]@
    deriving (Show, Eq)
instance Pretty MPlugin where
    pretty (MPlugin n ds)   = string "//" <+> pretty n
                                <//> string "{"  
                                <//> vcat (map pretty ds)
                                <//> string "}"

instance Pretty MDecl where
    pretty (MGlobal n a)    = string n <+> doubleQuotes (asStr a)
        where
            asStr (MStr s) = string s
            asStr a        = pretty a
    pretty (MMethod n vs a) = string n <+> string "\"" <-> parens (sepBy (string ", ") $ map string vs) <+> string "{"
                                    <//> indent 2 (vcat $ map pretty a) 
                                    <//> indent 1 (string "}" <-> string "\"") 

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

    -- pretty (MSwitch b cs d)     = error "TODO"

    pretty (MAssign n a)        = (pretty n <+> indent 10 (string "=" <+> pretty a) <-> string ";")
    pretty (MReturn a)          = (string "return" <+> pretty a) <> string ";"
    pretty (MExp a)             = (pretty a) <> string ";"
    pretty (MEmpty)             = string ";"

instance Pretty MExp where
    pretty (MOp1 n a)   = pretty n <+> pretty a
    pretty (MOp2 n a b) = pretty a <+> string n <+> pretty b
    pretty (MCall n as) = pretty n <> parens (sepBy (string ", ") $ map pretty as)
    pretty (MVar n)     = pretty n
    pretty (MInl c)     = string c
    pretty (MStr s)     = quotes (string $ s) -- TODO proper escaping
    pretty (MNum a)     = double a
    pretty (MBool a)    = if a then string "true" else string "false"
    pretty (MSelf)      = string "Self"
    pretty (MNull)      = string "null"

instance Pretty MVar where
    pretty (MId n)        = string n
    pretty (MProp n a)    = pretty n <> string "." <> string a
    pretty (MPropDef n a) = pretty n <> string "._property:" <> string a
    pretty (MIndex n a)   = pretty n <> brackets (pretty a)


-------------------------------------------------------------------------

-- |
-- Plugin generation monad including:
--  * A state for counting the number of generated functions
--  * A writer for collecting the generated functions
type MGen = WriterT [MDecl] (State Int)

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

execMGen :: MGen () -> [MDecl]
execMGen x = evalState (execWriterT x) 0

-------------------------------------------------------------------------



indent n = nest (4 * n)
spaces x = mempty <+> x <+> mempty x
concatWith x = mconcat . intersperse x

