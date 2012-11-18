
module Language.Baritone.Readers.Haskell (
         -- ** Haskell to core translation
         fromHaskell,

  ) where

import Control.Monad.Writer hiding ((<>))
import Control.Monad.State
import Data.Semigroup
import Data.Bits
import Data.List (intersperse, partition, union, (\\))
import Data.List.Split (splitOn)

import Language.Baritone.Core
import Language.Haskell.Syntax

-------------------------------------------------------------------------
-- Modules and declarations
-------------------------------------------------------------------------

-- |
-- Compiles a Haskell module into a Baritone module.
--
fromHaskell :: HsModule -> BModule
fromHaskell (HsModule l n es is as)
    = BModule (handleName n) (handleImports is) (handleDeclarations as)
    where
        handleName          = transModName
        handleImports       = map transImSpec
        handleDeclarations  = map transDec

transModName :: Module -> BModuleName
transModName (Module n) = splitOn "." n

-- No Baritone representation for these yet
transExSpec :: HsExportSpec -> ()
transExSpec = notSupported "Export list"

transImSpec :: HsImportDecl -> BImp
transImSpec = notSupported "Import declaration"

transDec :: HsDecl -> BDecl
transDec (HsPatBind l p a ws)              = transPatBind p a ws
transDec (HsFunBind [HsMatch l n ps a ws]) = translateFunBind n ps a ws
transDec (HsFunBind _)  = notSupported "Multiple bindings"
transDec _              = notSupported "Type, class or instance declaration"

-- | 
-- Translate a pattern binding (a.k.a constant definition)
--
transPatBind :: HsPat -> HsRhs -> [HsDecl] -> BDecl
transPatBind p a [] = BDecl (transPat p) (transRhs a)
transPatBind _ _ _  = notSupported "Where-clause"

-- | 
-- Translate a single function binding
--
translateFunBind :: HsName -> [HsPat] -> HsRhs -> [HsDecl] -> BDecl
translateFunBind n ps a [] = BDecl (getHsName n) (BAbs (map transPat ps) (transRhs a))
translateFunBind _ _  _ _  = notSupported "Where-clause"

-----------------------------------------------------------------
-- Patterns
-----------------------------------------------------------------

transPat :: HsPat -> BName
transPat (HsPVar n) = getHsName n
transPat _          = notSupported "Pattern matching"

-----------------------------------------------------------------
-- Guards
-----------------------------------------------------------------

transRhs :: HsRhs -> BExp
transRhs (HsUnGuardedRhs a) = transExp a
transRhs _                  = notSupported "Guards"

-----------------------------------------------------------------
-- Expressions
-----------------------------------------------------------------

transExp :: HsExp -> BExp
-- core
transExp (HsParen a)               = transExp a
transExp (HsVar n)                 = transQName n
transExp (HsApp a b)               = transAppOrInline a b
transExp (HsNegApp a)              = transNeg a
transExp (HsInfixApp a f b)        = transBinOp f a b
transExp (HsLeftSection a f)       = transLeftOp f a
transExp (HsRightSection f a)      = transRightOp f a
transExp (HsLambda l ps a)         = transLambda ps a
-- literals
transExp (HsLit l)                 = transLit l
-- special
transExp (HsCon n)                 = transQName n
transExp (HsTuple as)              = transTuple as
transExp (HsList as)               = transList as
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

-- Inline code                                                     
-- | 
--- We overload ordinary application syntax to mean inline as well,
-- so this function disambigues
transAppOrInline :: HsExp -> HsExp -> BExp
transAppOrInline a b 
    | isInline a = transInline b 
    | otherwise  = transApp a b

isInline :: HsExp -> Bool
isInline (HsVar (UnQual (HsIdent "inline"))) = True
isInline _                                   = False

transInline :: HsExp -> BExp
transInline (HsList ((HsLit (HsString c)):as)) = BInl c (map transExp as)
transInline _ = error "Inline needs a list of code and arguments"

-- Application
transApp :: HsExp -> HsExp -> BExp
transApp a b = BApp (transExp a) [transExp b]

transLeftOp :: HsQOp -> HsExp -> BExp
transLeftOp f a = BApp (transOp f) [transExp a]

transRightOp :: HsQOp -> HsExp -> BExp
transRightOp f a = BApp (BApp (BVar flipName) [transOp f]) [transExp a]

transBinOp :: HsQOp -> HsExp -> HsExp -> BExp
transBinOp f a b = BApp (transOp f) [transExp a, transExp b]

transNeg :: HsExp -> BExp
transNeg a = BApp (BVar negName) [transExp a]

-- Lambdas
transLambda :: [HsPat] -> HsExp -> BExp
transLambda ps a = BAbs (map transPat ps) (transExp a)



-- Names etc
mangle :: String -> String
mangle = id -- TODO


transQName :: HsQName -> BExp
transQName (Qual m n)                = notSupported "Qualified names"
transQName (UnQual n)                = BVar (mangle . getHsName $ n)
transQName (Special HsUnitCon)       = BVar unitName
transQName (Special HsListCon)       = BVar nilName
transQName (Special HsFunCon)        = BVar funcName
transQName (Special HsCons)          = BVar consName
transQName (Special (HsTupleCon n))  = BVar (cName n)

transOp :: HsQOp -> BExp
transOp = transQName . getHsQOp

getHsQOp :: HsQOp -> HsQName
getHsQOp (HsQVarOp n)  = n
getHsQOp (HsQConOp n)  = n
getHsName (HsIdent n)  = n
getHsName (HsSymbol n) = n

transLit :: HsLiteral -> BExp
transLit (HsChar c)     = BStr [c]
transLit (HsString s)	= BStr s
transLit (HsInt i)	    = BNum (fromIntegral i)
transLit (HsFrac r)	    = BNum (fromRational r)
transLit _              = notSupported "Unboxed literals"


-- Special syntax            
transTuple :: [HsExp] -> BExp
transTuple [] = BVar unitName
transTuple as = BApp (BVar . cName $ length as) (map transExp as)

transList :: [HsExp] -> BExp
transList []     = BVar nilName
transList (a:as) = BApp (BVar consName) [transExp a, transList as]

negName     = "__neg"
unitName    = "__unit"
nilName     = "__nil"
consName    = "__cons"
funcName    = "__func"
flipName    = "__flip"
cName n     = "__c" ++ show n


-----------------------------------------------------------------

notSupported m = error $ "Unsupported Haskell feature: " ++ m
