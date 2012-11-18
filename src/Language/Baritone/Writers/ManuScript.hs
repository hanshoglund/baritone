
module Language.Baritone.Writers.ManuScript (
         -- ** Core to ManuScript translation
         toManuScript
  ) where

import Control.Monad.Writer hiding ((<>))
import Control.Monad.State
import Data.Semigroup
import Numeric(showHex)
import Data.Char
import Data.List (intersperse, partition, union, (\\))
import Text.Pretty

import Language.Baritone.Core
import Language.ManuScript


-------------------------------------------------------------------------
-- Core to ManuScript
-------------------------------------------------------------------------

-- | Compile a Baritone module into a ManuScript plugin.
toManuScript :: BModule -> MPlugin
toManuScript (BModule n is as)
    = MPlugin
        (transModName n)
        (foldGlobals . execMGen $ mapM transValueDecl as >> return ())

transModName :: BModuleName -> MName
transModName = concatWith "_"

-- | Rewrite globals as self._property assignment
foldGlobals :: [MDecl] -> [MDecl]
foldGlobals xs = handleGlobals gs ++ ms
    where
        (gs, ms) = partition isGlobal xs

        handleGlobals :: [MDecl] -> [MDecl]
        handleGlobals x = [MMethod "Initialize" [] (mconcat $ map toSelfAssign x)]

        toSelfAssign :: MDecl -> [MStm]
        toSelfAssign (MGlobal n a) = [MAssign (MPropDef MSelf n) a]

transValueDecl :: BDecl -> MGen ()
transValueDecl (BDecl n a) = do        
    let coreTransform = mapVar mangle
    a' <- transExp True (coreTransform a)
    addGlobal (mangle n) a'
    return ()


transExp :: Bool -> BExp -> MGen MExp

transExp _     (BNum a) = return $ MNum a
transExp _     (BStr s) = return $ MStr s

transExp isTop (BVar n) = do
    let context = if isTop then MSelf else mid ctName
    return $ MVar (MProp context n)

transExp isTop (BApp f as) = do
    f'  <- transExp isTop f
    as' <- mapM (transExp isTop) as
    return $ MCall (MVar $ MProp f' apName) as'

transExp isTop (BInl c as) = do
    as' <- mapM (transExp isTop) as
    return $ MInl c as'

transExp isTop (BAbs ns a) = do
    let context = if isTop then MSelf else mid ctName
    a' <- transExp False a
    
    -- This generated function contains the body of the lambda abstraction
    -- It transfers all received variables into the current context before
    -- invoking the actual body. This could be optimized if BVar was aware of its scope
    invoke <- let
        vars = [ctName] ++ ns
        body =
            map (\n -> MAssign (MPropDef (mid ctName) n) (mid n)) ns
            ++
            [MReturn a']
        in addUniqueMethod vars body
    
    -- This generated function allocates and returns the function object
    -- It is invoked once, from context containing the lambda abstraction
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


mapVar :: (String -> String) -> BExp -> BExp
mapVar f (BVar a)    = (BVar $ f a)
mapVar f (BApp a as) = BApp (mapVar f a) (map (mapVar f) as)
mapVar f (BInl c as) = BInl c (map (mapVar f) as)
mapVar f (BAbs ns a) = BAbs ns (mapVar f a)
mapVar f x           = x

ctName    = "c"
allocName = "k"
apName    = "A"
  
mangle :: String -> String
mangle = concatMap f
    where                
        isVarLetter c = isAlphaNum c || c == '_'
        f c | isVarLetter c = [c]
            | otherwise    = "__" ++ showHex (ord c) "" ++ "__"

-- Primitive foldl
-- __foldl "(c, f, z, coll) {     
--         r = z;
--         for each x in xs {
--             z = f.A(z).A(x);
--         }
--         return r;
--     }"

-- Primitive run
-- Run "() {
--         Self.run.A(null);
--     }"

              
-------------------------------------------------------------------------

concatWith x = mconcat . intersperse x 