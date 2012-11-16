
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

concatWith x = mconcat . intersperse x 

