
module Main where

import System.IO
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
    let b  = toCore hs
    let ms = fromCore b
    
    hPutStr output $ show (pretty ms)
    hPutStr output "\n"
    
    return ()