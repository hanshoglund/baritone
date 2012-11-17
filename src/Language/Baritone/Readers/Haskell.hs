
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
transDec (HsFunBind [HsMatch l n ps a ws]) = translateFunBind n ps a ws
transDec (HsFunBind _) = notSupported "Multiple bindings"
transDec _             = notSupported "Type, class or instance declaration"

-----------------------------------------------------------------

transPatBind :: HsPat -> HsRhs -> [HsDecl] -> BDecl
transPatBind p a [] = BDecl (transPat p) (transRhs a)
transPatBind _ _ _  = notSupported "Where-clause"

translateFunBind :: HsName -> [HsPat] -> HsRhs -> [HsDecl] -> BDecl
translateFunBind n ps a ws = BDecl (getHsName n) (BAbs (map transPat ps) (transRhs a))

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
transExp (HsApp a b)               = if isInline a then transInline b else transApp a b
transExp (HsNegApp a)              = transNeg a
transExp (HsInfixApp a f b)        = transOp2 f a b
transExp (HsLeftSection a f)       = transOp f a
transExp (HsRightSection f a)      = transOpFlip f a
transExp (HsLambda l ps a)         = transLambda ps a
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

transNeg :: HsExp -> BExp
transNeg a = BApp (BVar negName) [transExp a]

transApp :: HsExp -> HsExp -> BExp
transApp a b = BApp (transExp a) [transExp b]

transOp :: HsQOp -> HsExp -> BExp
transOp f a = BApp (transQName . getHsQOp $ f) [transExp a]

transOpFlip :: HsQOp -> HsExp -> BExp
transOpFlip f a = BApp (BApp (BVar flipName) [transQName . getHsQOp $ f]) [transExp a]

transOp2 :: HsQOp -> HsExp -> HsExp -> BExp
transOp2 f a b = BApp (transQName . getHsQOp $ f) [transExp a, transExp b]

transLambda :: [HsPat] -> HsExp -> BExp
transLambda ps a = BAbs (map transPat ps) (transExp a)

-- We overload application to mean inline in case it matches this
isInline :: HsExp -> Bool
isInline (HsVar (UnQual (HsIdent "inline"))) = True
isInline _                                   = False

transInline :: HsExp -> BExp
transInline (HsList ((HsLit (HsString c)):as)) = BInl c (map transExp as)
transInline _ = error "Inline needs a list of code and arguments"

transQName :: HsQName -> BExp
transQName (UnQual n)                = BVar (getHsName n)
transQName (Qual m n)                = notSupported "Qualified names"
transQName (Special HsUnitCon)       = BVar unitName
transQName (Special HsListCon)       = BVar emptyName
transQName (Special HsFunCon)        = BVar funcName
transQName (Special HsCons)          = BVar (cName 2)
transQName (Special (HsTupleCon n))  = BVar (cName n)

transLit :: HsLiteral -> BExp
transLit (HsChar c)     = BStr [c]
transLit (HsString s)	= BStr s
transLit (HsInt i)	    = BNum (fromIntegral i)
transLit (HsFrac r)	    = BNum (fromRational r)
transLit _              = notSupported "Unboxed literals"

getHsQOp :: HsQOp -> HsQName
getHsQOp (HsQVarOp n)  = n
getHsQOp (HsQConOp n)  = n
getHsName (HsIdent n)  = n
getHsName (HsSymbol n) = n

negName     = "__neg"
unitName    = "__unit"
emptyName   = "__empty"
funcName    = "__func"
flipName    = "__flip"
cName n     = "__c" ++ show n


-----------------------------------------------------------------

notSupported m = error $ "Unsupported Haskell feature: " ++ m
