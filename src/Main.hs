
module Main where

import System.IO
import System.Exit
import System.Environment
import System.Console.GetOpt

import Text.Pretty

import Language.Haskell.Parser
import Language.Haskell.Syntax
import Language.Baritone


-- run as filter
main = do
    compileFile stdin stdout

parse :: String -> HsModule
parse s = case (parseModule s) of
    ParseOk hs -> hs
    ParseFailed (SrcLoc f l c) e -> error $ f ++ ":" ++ show l ++ ":" ++ show c ++ ": " ++ e

-- compile a single file (to core for now)
compileFile :: Handle -> Handle -> IO ()
compileFile input output = do
    s <- hGetContents input
    let hs = parse s
    let b  = transform (singleApp . singleAbs) $ fromHaskell hs
    let ms = toManuScript b
    
    hPutStr output $ show (pretty b)
    hPutStr output "\n"
    hPutStr output "\n"
    hPutStr output $ show (pretty ms)
    hPutStr output "\n"
    
    return ()