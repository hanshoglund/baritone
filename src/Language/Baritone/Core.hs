
{-# LANGUAGE StandaloneDeriving #-}

module Language.Baritone.Core (
         -- ** Baritone core language
         BModule(..),
         BName(..),
         BModuleName(..),
         BImp(..),
         BDecl(..),
         BExp(..),
         isBAbs,
         isBApp,
         freeVars,
         isFreeIn,

         -- ** Haskell to core translation
         toCore,

         -- ** Core to ManuScript translation
         fromCore
  ) where

import Control.Monad.Writer hiding ((<>))
import Control.Monad.State
import Data.Semigroup
import Data.List (intersperse, partition, union, (\\))
import Data.List.Split (splitOn)

import Language.Haskell.Syntax
import Language.ManuScript

import Text.Pretty


data BModule = BModule BModuleName [BImp] [BDecl] -- ^ /name imports declarations/
    deriving (Eq, Show)

type BName = String

type BModuleName = [BName]

data BImp = BImp BModuleName [BName] (Maybe BName) -- ^ /name hiding alias/
    deriving (Eq, Show)

data BDecl = BDecl String BExp
    deriving (Eq, Show)

data BExp
    = BVar BName        -- ^ /name/
    | BApp BExp [BExp]  -- ^ /function arguments/
    | BAbs [BName] BExp -- ^ /bindings body/
    | BInl String
    | BNum Double
    | BStr String
    deriving (Eq, Show)

isBApp (BApp _ _) = True
isBApp _          = False
isBAbs (BAbs _ _) = True
isBAbs _          = False

freeVars :: BExp -> [BName]
freeVars (BVar n)    = [n]
freeVars (BApp f as) = freeVars f `union` concatMap freeVars as
freeVars (BAbs ns a) = freeVars a \\ ns
freeVars _           = []

isFreeIn :: BName -> BExp -> Bool
isFreeIn n a = elem n (freeVars a)



-- | The 'Pretty' instance generates valid Haskell for all Baritione language constructs.
instance Pretty BModule where
    pretty (BModule n is as) = string "module" <+> string (concatWith "." n)
                                    <+> vcat (map pretty is)
                                    <+> string "where"
                                    <//> empty
                                    <//> vcat (map pretty as)
-- | The 'Pretty' instance generates valid Haskell for all Baritione language constructs.
instance Pretty BImp where
    pretty (BImp n hs a) = string "import" <+> string "hiding"
                                           <+> parens (sepBy (string ",") $ map pretty hs)
                                           <+> string "as" <+> maybe mempty string a
-- | The 'Pretty' instance generates valid Haskell for all Baritione language constructs.
instance Pretty BDecl where
    pretty (BDecl n a) = string n </> nest 12 (string "=" <+> pretty a)

-- | The 'Pretty' instance generates valid Haskell for all Baritione language constructs.
instance Pretty BExp where
    pretty (BInl s)     = string "inline" <+> string s
    pretty (BNum n)     = string (show n)
    pretty (BStr s)     = string (show s)
    pretty (BVar n)     = string n
    pretty (BAbs ns a)  = (string "\\" <-> hsep (map string ns) <+> string "->") <+> pretty a
    pretty (BApp f as)  = hsep (map p $ f:as)
        where
            p x | isBApp x  = parens (pretty x)
                | otherwise = pretty x

-------------------------------------------------------------------------
-- Haskell to Core
-------------------------------------------------------------------------

-- |
-- Compiles a Haskell module into a Baritone module.
--
toCore :: HsModule -> BModule
toCore (HsModule l n es is as) = BModule (handleName n) (handleImports is) (handleDeclarations as)

    where

        handleName          = transModName
        handleImports       = map transImSpec
        handleDeclarations  = map transDec

        transModName :: Module -> BModuleName
        transModName (Module n) = splitOn "." n

        transExSpec :: HsExportSpec -> ()
        transExSpec = notSupported "Export list"

        transImSpec :: HsImportDecl -> BImp
        transImSpec = notSupported "Import declaration"


        -----------------------------------------------------------------
        
        transDec :: HsDecl -> BDecl
        transDec (HsPatBind l p a ws)              = transPatBind p a ws
            where
                transPatBind :: HsPat -> HsRhs -> [HsDecl] -> BDecl
                transPatBind p a [] = BDecl (transPat p) (transRhs a)
                transPatBind _ _ _  = notSupported "With clause"

        transDec (HsFunBind [HsMatch l n ps a ws]) = translateFunBind n ps a ws
            where
                translateFunBind :: HsName -> [HsPat] -> HsRhs -> [HsDecl] -> BDecl
                translateFunBind n ps a ws = BDecl (getHsName n) (BAbs (map transPat ps) (transRhs a))

        transDec (HsFunBind _)             = notSupported "Multiple bindings"
        transDec _                         = notSupported "Type, class or instance declaration"
        
        -----------------------------------------------------------------


        transPat :: HsPat -> BName
        transPat (HsPVar n) = getHsName n
        transPat _          = notSupported "Pattern matching"

        transRhs :: HsRhs -> BExp
        transRhs (HsUnGuardedRhs a) = transExp a
        transRhs _                  = notSupported "Guards"

        
        -----------------------------------------------------------------
        
        transExp :: HsExp -> BExp
        transExp (HsParen a)               = transExp a

        -- core
        transExp (HsVar n)                 = transQName n
        transExp (HsApp f a)               = BApp (transExp f) [transExp a]
        transExp (HsNegApp a)              = BApp (BVar "__neg") [transExp a]
        transExp (HsInfixApp a f b)        = BApp (transQName . getHsQOp $ f) [transExp a, transExp b]
        transExp (HsLeftSection a f)       = BApp (transQName . getHsQOp $ f) [transExp a]
        transExp (HsRightSection f a)      = BApp (transQName . getHsQOp $ f) [transExp a]
        transExp (HsLambda l ps as)        = BAbs (map transPat ps) (transExp as)

        -- literals
        transExp (HsLit l)                 = transLit l

        -- special
        transExp (HsCon n)                 = notSupported "Constructors"
        transExp (HsTuple as)              = notSupported "Tuples"
        transExp (HsList as)               = notSupported "Lists"
        transExp (HsIf p a b)              = notSupported "If-expressions"
        transExp (HsCase p as)             = notSupported "Case-expressions"
        transExp (HsLet ds a)              = notSupported "Let-expressions"
        transExp (HsDo as)                 = notSupported "Do-expressions"

        -- sugar
        transExp (HsRecConstr n m)         = notSupported "Records"
        transExp (HsRecUpdate n m)         = notSupported "Records"
        transExp (HsEnumFrom a)            = notSupported "Enum syntax"
        transExp (HsEnumFromTo a b)        = notSupported "Enum syntax"
        transExp (HsEnumFromThen a b)      = notSupported "Enum syntax"
        transExp (HsEnumFromThenTo a b c)  = notSupported "Enum syntax"
        transExp (HsListComp a as)         = notSupported "List comprehensions"

        -- types
        transExp (HsExpTypeSig l a t)      = notSupported "Type signature"

        -- patterns
        transExp (HsAsPat n a)             = notSupported "As-patterns"
        transExp (HsWildCard)              = notSupported "Wildcards"
        transExp (HsIrrPat a)              = notSupported "Irrefutable patterns"

        -----------------------------------------------------------------

        transQName :: HsQName -> BExp
        transQName (UnQual n)                = BVar (getHsName n)
        transQName (Qual m n)                = notSupported "Qualified names"
        transQName (Special HsUnitCon)       = notSupported "Unit constructor"
        transQName (Special HsListCon)       = notSupported "List constructor"
        transQName (Special (HsTupleCon n))  = notSupported "Tuple constructors"
        transQName (Special HsFunCon)        = notSupported "Function type constructor"
        transQName (Special HsCons)          = notSupported "Cons expression"

        transLit :: HsLiteral -> BExp
        transLit (HsChar c)     = BStr [c]
        transLit (HsString s)	= BStr s
        transLit (HsInt i)	    = BNum (fromIntegral i)
        transLit (HsFrac r)	    = BNum (fromRational r)
        transLit _              = notSupported "Unboxed literals"

        getHsQOp (HsQVarOp n)  = n
        getHsQOp (HsQConOp n)  = n
        getHsName (HsIdent n)  = n
        getHsName (HsSymbol n) = n

        -----------------------------------------------------------------
        
        notSupported m = error $ "Unsupported Haskell feature: " ++ m

-------------------------------------------------------------------------
-- Core to ManuScript
-------------------------------------------------------------------------

-- | Compile a Baritone module into a ManuScript plugin.
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
                handleGlobals x = [MMethod "Initialize" [] (mconcat $ map toSelfAssign x)]

                toSelfAssign :: MDecl -> [MStm]
                toSelfAssign (MGlobal n a) = [MAssign (MPropDef MSelf n) a]

        transModName :: BModuleName -> MName
        transModName = concatWith "_"

        transValueDecl :: BDecl -> MGen ()
        transValueDecl (BDecl n a) = do
            a' <- transExp True (fixPrimOps a)
            addGlobal n a'
            return ()


        transExp :: Bool -> BExp -> MGen MExp
        transExp isTop (BVar n) = do
            let context = if isTop then MSelf else mid ctName
            return $ MVar (MProp context n)

        transExp isTop (BApp f as) = do
            f'  <- transExp isTop f
            as' <- mapM (transExp isTop) as
            return $ MCall (MVar $ MProp f' apName) as'

        transExp isTop (BAbs ns a) = do
            let context = if isTop then MSelf else mid ctName
            a' <- transExp False a
            invoke <- let
                vars = [ctName] ++ ns
                body =
                    map (\n -> MAssign (MPropDef (mid ctName) n) (mid n)) ns
                    ++
                    [MReturn a']
                in addUniqueMethod vars body
            create <- let
                vars = [ctName]
                alloc = [
                    MAssign (MId allocName) (MCall (mid "CreateDictionary") [])
                    ]
                copy = map (\n -> MAssign (MPropDef (mid allocName) n) (mprop (mid ctName) n)) (freeVars a \\ ns)
                ret = [
                    MExp (MCall (mprop (mid allocName) "SetMethod") [MStr apName, MSelf, MStr invoke]),
                    MReturn (mid allocName)
                    ]
                in addUniqueMethod vars (alloc ++ copy ++ ret)
            return $ MCall (mid create) [context]

        transExp _ (BNum a) = return $ MNum a
        transExp _ (BStr s) = return $ MStr s
        transExp _ (BInl c) = return $ MInl c


        fixPrimOps :: BExp -> BExp
        fixPrimOps (BVar f) = (BVar $ primOp f)
        fixPrimOps (BApp f as) = BApp (fixPrimOps f) (map fixPrimOps as)
        fixPrimOps (BAbs ns a) = BAbs ns (fixPrimOps a)
        fixPrimOps x           = x

        mid x     = MVar (MId x)
        mprop x y = MVar (MProp x y)

        primOp :: BName -> BName
        primOp "(+)" = "__add"
        primOp "(-)" = "__sub"
        primOp "(*)" = "__mul"
        primOp "(/)" = "__div"
        primOp "+"   = "__add"
        primOp "-"   = "__sub"
        primOp "*"   = "__mul"
        primOp "/"   = "__div"
        primOp x     = x

        ctName    = "c"
        allocName = "k"
        apName    = "A"

        -- FIXME has to be proper functions

        main :: [MDecl]
        main = [
                MMethod "Run" [] [MExp $ MCall (MVar $ MProp (mid "main") apName) []]
            ]

        primOps :: [MDecl]
        primOps
            = [
                MMethod "add"   ["a", "b"] [MReturn $ MOp2 "+" (mid "a") (mid "b")],
                MMethod "sub"   ["a", "b"] [MReturn $ MOp2 "-" (mid "a") (mid "b")],
                MMethod "mul"   ["a", "b"] [MReturn $ MOp2 "*" (mid "a") (mid "b")],
                MMethod "div"   ["a", "b"] [MReturn $ MOp2 "/" (mid "a") (mid "b")]
              ]

-------------------------------------------------------------------------

concatWith x = mconcat . intersperse x

