
{-# LANGUAGE StandaloneDeriving #-}

module Language.ManuScript (
         -- ** ManuScript language
         MPlugin(..),
         MName(..),
         MOpName(..),
         MDecl(..),
         MStm(..),
         MExp(..),
         MVar(..),
         isMGlobal,
         
         -- *** Code generation monad
         MGen(..),
         addGlobal,
         addMethod,
         addUniqueMethod,
         execMGen,
         createPlugin,
  ) where

import Control.Monad.Writer hiding ((<>))
import Control.Monad.State
import Data.Semigroup
import Data.List (intersperse, partition, union, (\\))
import Data.List.Split (splitOn)
import Text.Pretty

-------------------------------------------------------------------------
-- ManuScript
-------------------------------------------------------------------------

-- |
-- A ManuScript plugin
--
-- See <http://www.simkin.co.uk/Docs/java>
--
data MPlugin = MPlugin MName [MDecl] -- ^ /name declarations/
    deriving (Show, Eq)

type MName   = String
type MOpName = String

data MDecl
    = MGlobal MName MExp            -- ^ /name body/
    | MMethod MName [MName] [MStm]  -- ^ /name vars body/
    deriving (Show, Eq)

isMGlobal (MGlobal _ _) = True
isMGlobal _             = False

data MStm
    = MIf       MExp [MStm] [MStm]                      -- ^ /test true-case false-case/
    | MWhile    MExp [MStm]                             -- ^ /test body/
    | MFor      MName MExp MExp (Maybe MExp) [MStm]     -- ^ /var from to step body/
    | MForEach  (Maybe MName) MName MExp [MStm]         -- ^ /type? var iterable body/
    | MSwitch   MExp [(MExp, [MStm])] (Maybe [MStm])    -- ^ /disamb cases default/
    | MAssign   MVar MExp                               -- ^ /var expr/
    | MReturn   MExp                                    -- ^ /expr/
    | MExp      MExp                                    -- ^ /expr/
    | MEmpty
    deriving (Show, Eq)

data MExp
    = MOp1      MOpName MExp        -- ^ /op expr/
    | MOp2      MOpName MExp MExp   -- ^ /op expr expr/
    | MCall     MExp [MExp]         -- ^ /expr expr*/
    | MInl      String [MExp]       -- ^ /code expr*/
    | MVar      MVar                -- ^ /var/
    | MStr      String
    | MNum      Double
    | MBool     Bool
    | MSelf
    | MNull
    deriving (Show, Eq)

data MVar
    = MId       MName
    | MProp     MExp MName -- ^ /a.n/
    | MPropDef  MExp MName -- ^ /a._property:n/
    | MIndex    MExp MExp  -- ^ /a[n]/
    deriving (Show, Eq)

instance Pretty MPlugin where
    pretty (MPlugin n ds)   = string "{"
                                <//> vcat (map pretty ds)
                                <//> string "}"

instance Pretty MDecl where
    pretty (MGlobal n a) = string n <+> doubleQuotes (f a)
        where
            f (MStr s) = string s
            f a        = pretty a

    pretty (MMethod n vs a)     = string n <+> string "\"" <-> parens (sepBy (string ", ") $ map string vs) <+> string "{"
                                    <//> indent 2 (vcat $ map pretty a)
                                    <//> indent 1 (string "}" <-> string "\"")

instance Pretty MStm where
    pretty (MIf p a b)          = string "if" <+> parens (pretty p)
                                  <//> string "{" <//> indent 1 (pretty a) <//> string "}"
                                  <//> string "else"
                                  <//> string "{" <//> indent 1 (pretty a) <//> string "}"

    pretty (MWhile p a)         = string "while" <+> parens (pretty p)
                                  <//> string "{" <//> indent 1 (vcat $ map pretty a) <//> string "}"

    pretty (MFor v i j k a)     = string "for" <+> parens clause
                                  <//> string "{" <//> indent 1 (vcat $ map pretty a) <//> string "}"
        where clause            = pretty v
                                  <+> string "=" <+> pretty i
                                  <+> string "to" <+> pretty j
                                  <+> maybe mempty ((string "step" <+>) . pretty) k

    pretty (MForEach t v u a)   = string "for" <+> string "each" <+> clause
                                  <//> string "{" <//> indent 1 (vcat $ map pretty a) <//> string "}"
        where clause            = maybe mempty pretty t
                                  <+> pretty v
                                  <+> string "in" <+> pretty u

    pretty (MSwitch a cs d)     = string "switch" <+> parens (pretty a)
                                  <//> string "{"
                                  <//> indent 1 (vcat $ map caseClause cs)
                                  <//> maybe mempty defaultClause d
                                  <//> string "}"
        where caseClause (c,as) = string "case" <+> pretty c <+> braces (vcat $ map pretty as)
              defaultClause as  = string "default" <+> braces (vcat $ map pretty as)

    pretty (MAssign n a)        = (pretty n <+> indent 10 (string "=" <+> pretty a) <-> string ";")
    pretty (MReturn a)          = (string "return" <+> pretty a) <> string ";"
    pretty (MExp a)             = (pretty a) <> string ";"
    pretty (MEmpty)             = string ";"

instance Pretty MExp where
    pretty (MOp1 n a)   = pretty n <+> pretty a
    pretty (MOp2 n a b) = pretty a <+> string n <+> pretty b
    pretty (MCall n as) = pretty n <> parens (sepBy (string ", ") $ map pretty as)
    pretty (MInl c as)  = hcat $ interfold (map string $ splitOn "x" c) (map pretty as)
    pretty (MVar n)     = pretty n
    pretty (MStr s)     = quotes (string $ s) -- TODO proper escaping
    pretty (MNum a)     = double a
    pretty (MBool a)    = if a then string "true" else string "false"
    pretty (MSelf)      = string "Self"
    pretty (MNull)      = string "null"

instance Pretty MVar where
    pretty (MId n)        = string n
    pretty (MProp n a)    = pretty n <> string "." <> string a
    pretty (MPropDef n a) = pretty n <> string "._property:" <> string a
    pretty (MIndex n a)   = pretty n <> brackets (pretty a)


-------------------------------------------------------------------------

-- |
-- Plugin generation monad, including:
--
--  * 'State' for counting the number of generated functions
--
--  * 'WriterT' for collecting the generated functions
--
type MGen = WriterT [MDecl] (State Int)

addGlobal :: MName -> MExp -> MGen ()
addGlobal n e = do
    tell [MGlobal n e]
    return ()

addMethod :: MName -> [MName] -> [MStm] -> MGen ()
addMethod n vs as = do
    tell [MMethod n vs as]
    return ()

addUniqueMethod :: [MName] -> [MStm] -> MGen MName
addUniqueMethod vs as = do
    c <- get
    put (succ c)
    let n = "__" ++ show c
    tell [MMethod n vs as]
    return n

execMGen :: MGen () -> [MDecl]
execMGen x = evalState (execWriterT x) 0

createPlugin :: MGen () -> MPlugin
createPlugin = MPlugin "" . execMGen

-------------------------------------------------------------------------

indent n = nest (4 * n)
concatWith x = mconcat . intersperse x
interleave as bs = concat $ zipWith (\x y -> [x,y]) as bs
interfold as bs = interleave as bs ++ take 1 (drop (length bs) as)


