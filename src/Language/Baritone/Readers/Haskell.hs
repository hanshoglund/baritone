
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
transExp (HsVar n)                 = transName n
transExp (HsApp a b)               = transAppOrInline a b
transExp (HsNegApp a)              = transNeg a
transExp (HsInfixApp a f b)        = transBinOp f a b
transExp (HsLeftSection a f)       = transLeftOp f a
transExp (HsRightSection f a)      = transRightOp f a
transExp (HsLambda l ps a)         = transLambda ps a
-- literals
transExp (HsLit l)                 = transLit l
-- special
transExp (HsCon n)                 = transName n
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

-- | We overload ordinary application syntax to mean inline as well,
-- so this function disambigues
transAppOrInline :: HsExp -> HsExp -> BExp
transAppOrInline a b 
    | isInline a = transInline b 
    | otherwise  = transApp a b

isInline :: HsExp -> Bool
isInline (HsVar (UnQual (HsIdent kw))) = (kw == inlineKeyword)
isInline _ = False

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

-- | Translate a variable name                   
--
-- Note that operators are rendered verbatim, i.e. both @1 + 2@ and @(+) 1 2@
-- gives @+@ as the name of the applied function.
--
transName :: HsQName -> BExp
transName (Qual m n)                = notSupported "Qualified names"
transName (UnQual n)                = BVar (getHsName $ n)
transName (Special HsUnitCon)       = BVar unitName
transName (Special HsListCon)       = BVar nilName
transName (Special HsFunCon)        = BVar funcName
transName (Special HsCons)          = BVar consName
transName (Special (HsTupleCon n))  = BVar (cName n)

transOp :: HsQOp -> BExp
transOp = transName . getHsQOp

-- mapHsQName :: (String -> String) -> HsQName -> HsQName
-- mapHsQName f (Qual m n) = Qual m (mapHsName f n)
-- mapHsQName f (UnQual n) = UnQual (mapHsName f n)
-- mapHsQName f (Special n) = (Special n)
-- 
-- mapHsName :: (String -> String) -> HsName -> HsName
-- mapHsName f (HsIdent n)  = HsIdent (f n)
-- mapHsName f (HsSymbol n) = HsSymbol (f n)

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


-- Special constructors            

transTuple :: [HsExp] -> BExp
transTuple [] = BVar unitName
transTuple as = BApp (BVar . cName $ length as) (map transExp as)

transList :: [HsExp] -> BExp
transList []     = BVar nilName
transList (a:as) = BApp (BVar consName) [transExp a, transList as]


-----------------------------------------------------------------
-- Data constructors
-----------------------------------------------------------------

-- | Only the HsDataDecl constructor
type HsData = HsDecl

-- | Generates the destructor function for an algebraic type.
--
-- The destruction function receives a matching clause for each constructor and finally
-- the actual value, for example @__maybe :: b -> (a -> b) -> Maybe a -> b@
--
makeDest :: HsData -> BDecl
makeDest (HsDataDecl l c n vs cs ds) 
    = BDecl name expr    
    where
        -- The name is __T for data type t etc.
        name = "__" ++ getHsName n

        -- As each data type /is/ its destructor we simply shuffle the arguments:
        --
        -- > __T d1 d2 ... dN a = a d1 d2 ... dN
        --
        -- Empty data declarations can not be destructed, so their destructor is undefined.
        num  = length cs
        ns   = map (\i -> "d" ++ show i) [1..num]
        expr = BAbs (ns ++ ["a"]) (BApp (BVar "a") (map BVar ns))


-- | Generates the constructor functions for an algebraic type.
-- Each constructor receives all matching clauses and invoke the correct one, for example
-- @__Just :: b -> (a -> b)
makeCons :: HsData -> [BDecl]
makeCons (HsDataDecl l c n vs cs ds) = 
    mapIndexed (\i c -> BDecl ("__" ++ getHsName n) $ makeSingleCons (length cs) i (conDeclNumArgs c)) cs
    where
        -- The name is __Nothing, __Just etc.

        makeSingleCons :: Int -> Int -> Int -> BExp
        makeSingleCons numClauses clauseIndex numArgs
            = BAbs (args ++ clauses) body -- FIXME if args is empty?
            where
                args    = map (\i -> "a" ++ show i) [1..numArgs]    -- may be empty
                clauses = map (\i -> "c" ++ show i) [1..numClauses]
                clause  = clauses !! clauseIndex
                body | length args <= 0 = BVar clause
                     | otherwise        = BApp (BVar clause) (map BVar args)

conDeclNumArgs :: HsConDecl -> Int
conDeclNumArgs (HsConDecl l n as) = length as
conDeclNumArgs (HsRecDecl l n fs) = length fs

-----------------------------------------------------------------
-- Names
-----------------------------------------------------------------

inlineKeyword = "inline"
negName       = "__neg"
unitName      = "__unit"
nilName       = "__nil"
consName      = "__cons"
funcName      = "__func"
flipName      = "__flip"
cName n       = "__c" ++ show n

-----------------------------------------------------------------

notSupported m = error $ "Unsupported Haskell feature: " ++ m

mapIndexed :: (Int -> a -> b) -> [a] -> [b]
mapIndexed f as = map (uncurry f) (zip is as)
    where
        n  = length as - 1
        is = [0..n]


