
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
    a' <- transExp True (fixPrimOps $ a)
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

transExp isTop (BInl c as) = do
    as' <- mapM (transExp isTop) as
    return $ MInl c as'

transExp _ (BNum a)    = return $ MNum a
transExp _ (BStr s)    = return $ MStr s


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