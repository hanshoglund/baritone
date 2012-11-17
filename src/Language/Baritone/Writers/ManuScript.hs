
module Language.Baritone.Writers.ManuScript (
         -- ** Core to ManuScript translation
         toManuScript
  ) where

import Control.Monad.Writer hiding ((<>))
import Control.Monad.State
import Data.Semigroup
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
    a' <- transExp True (primOps $ a)
    addGlobal n a'
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


primOps :: BExp -> BExp
primOps (BVar f)    = (BVar $ primOp f)
primOps (BApp f as) = BApp (primOps f) (map primOps as)
primOps (BAbs ns a) = BAbs ns (primOps a)
primOps x           = x

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

              
-------------------------------------------------------------------------

concatWith x = mconcat . intersperse x 