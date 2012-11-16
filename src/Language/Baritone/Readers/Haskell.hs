
module Language.Baritone.Readers.Haskell (
         -- ** Haskell to core translation
         fromHaskell,

  ) where

import Control.Monad.Writer hiding ((<>))
import Control.Monad.State
import Data.Semigroup
import Data.List (intersperse, partition, union, (\\))
import Data.List.Split (splitOn)

import Language.Baritone.Core
import Language.Haskell.Syntax

-------------------------------------------------------------------------
-- Haskell to Core
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

transExSpec :: HsExportSpec -> ()
transExSpec = notSupported "Export list"

transImSpec :: HsImportDecl -> BImp
transImSpec = notSupported "Import declaration"

transDec :: HsDecl -> BDecl
transDec (HsPatBind l p a ws) = transPatBind p a ws
    where
        transPatBind :: HsPat -> HsRhs -> [HsDecl] -> BDecl
        transPatBind p a [] = BDecl (transPat p) (transRhs a)
        transPatBind _ _ _  = notSupported "With clause"

transDec (HsFunBind [HsMatch l n ps a ws]) = translateFunBind n ps a ws
    where
        translateFunBind :: HsName -> [HsPat] -> HsRhs -> [HsDecl] -> BDecl
        translateFunBind n ps a ws = BDecl (getHsName n) (BAbs (map transPat ps) (transRhs a))

transDec (HsFunBind _) = notSupported "Multiple bindings"
transDec _             = notSupported "Type, class or instance declaration"

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
