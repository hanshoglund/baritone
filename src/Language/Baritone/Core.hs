
{-# LANGUAGE StandaloneDeriving #-}

-- |
-- The baritone language is a variant of the lambda calculus with numbers and strings.
--
module Language.Baritone.Core (
         -- ** Core language
         BModule(..),
         BName(..),
         BModuleName(..),
         BImp(..),
         BDecl(..),
         BExp(..),

         -- ** Predicates
         freeVars,
         isFreeIn,
         isBAbs,
         isBApp,

         -- ** Transformations
         compoundApp,
         singleApp,
         compoundAbs,
         singleAbs,            
         transform,
  ) where

import Control.Monad.Writer hiding ((<>))
import Control.Monad.State
import Data.Semigroup
import Data.List (intersperse, partition, union, (\\))

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
    | BAbs [BName] BExp -- ^ /bindings+ body/
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

transform :: (BExp -> BExp) -> BModule -> BModule
transform f (BModule n is ds) = BModule n is (map (t f) ds)
    where
        t f (BDecl n d) = BDecl n (f d)

deep :: (BExp -> BExp) -> BExp -> BExp
deep f (BApp a as) = BApp (f a) (map (deep f) as)
deep f (BAbs ns a) = BAbs ns (f a)
deep f x           = x

-- | 
-- Transforms the given expression to use compound application, that is:
--
-- * @(f [x]) [y]@ is forbidden
-- 
-- * @f [x y]@ is allowed
-- 
-- * 'BApp' may not have a 'BApp' head
-- 
-- * 'BApp' may have multiple arguments
-- 
compoundApp :: BExp -> BExp
compoundApp = f
    where
        f (BApp (BApp a as) bs) = f $ BApp a (map f $ as ++ bs)
        f (BApp a as)           = BApp (f a) (map f as)
        f (BAbs ns a)           = BAbs ns (f a)
        f x                     = x


-- | 
-- Transforms the given expression to use single application, that is:
--
-- * @f [x y]@ is forbidden
-- 
-- * @(f [x]) [y]@ is allowed
-- 
-- * 'BApp' may have a 'BApp' head
-- 
-- * All 'BApp' instances have a single argument
-- 
singleApp :: BExp -> BExp
singleApp = f
    where
        f (BApp a [b])    = BApp (f a) [f b]
        f (BApp a (b:bs)) = f $ BApp (BApp a [b]) (map f bs)
        f (BAbs ns a)     = BAbs ns (f a)
        f x               = x

-- | 
-- Transforms the given expression to use compound abstraction, that is:
--
-- * @\\x -> \\y -> z@ is forbidden
-- 
-- * @\\x y -> z@ is allowed
-- 
-- * 'BAbs' may not have a 'BAbs' body
-- 
-- * 'BAbs' instances may have multiple bindings
-- 
compoundAbs :: BExp -> BExp
compoundAbs = f
    where
        f (BAbs ms (BAbs ns a)) = BAbs (ms ++ ns) (f a)
        f (BAbs ns a)           = BAbs ns (f a)
        f (BApp a as)           = BApp (f a) (map f as)
        f x                     = x

-- | 
-- Transforms the given expression to use single abstraction, that is:
-- 
-- * @\\x y -> z@ is forbidden
--
-- * @\\x -> \\y -> z@ is allowed
-- 
-- * 'BAbs' may have a 'BAbs' body
-- 
-- * All 'BAbs' instances have single bindings
-- 
singleAbs :: BExp -> BExp
singleAbs = f
    where
        f (BAbs [n] a)          = BAbs [n] (f a)
        f (BAbs (n:ns) a)       = BAbs [n] (f $ BAbs ns a)
        f (BApp a as)           = BApp (f a) (map f as)
        f x                     = x






-- | The 'Pretty' instance generates valid Haskell for all Baritone language constructs.
instance Pretty BModule where
    pretty (BModule n is as) = string "module" <+> string (concatWith "." n)
                                               <+> vcat (map pretty is)
                                               <+> string "where"
                                               <//> empty
                                               <//> vcat (map pretty as)

-- | The 'Pretty' instance generates valid Haskell for all Baritone language constructs.
instance Pretty BImp where
    pretty (BImp n hs a) = string "import" <+> string "hiding"
                                           <+> parens (sepBy (string ",") $ map pretty hs)
                                           <+> string "as" <+> maybe mempty string a

-- | The 'Pretty' instance generates valid Haskell for all Baritone language constructs.
instance Pretty BDecl where
    pretty (BDecl n a) = string n </> nest 12 (string "=" <+> pretty a)


-- | The 'Pretty' instance generates valid Haskell for all Baritone language constructs.
instance Pretty BExp where
    pretty (BInl s)     = string "inline" <+> string s
    pretty (BNum n)     = string (show n)
    pretty (BStr s)     = string (show s)
    pretty (BVar n)     = string n
    pretty (BAbs ns a)  = (string "\\" <-> hsep (map string ns) <+> string "->") <+> pretty a
    pretty (BApp f as)  = hsep (map p $ f:as)
        where
            p x | isBApp x  = parens (pretty x)
                | isBAbs x  = parens (pretty x)
                | otherwise = pretty x


-------------------------------------------------------------------------

concatWith x = mconcat . intersperse x 

