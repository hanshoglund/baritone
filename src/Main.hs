
module Main where

import System.IO
import Language.Haskell.Parser
import Language.Baritone.Core

-- run as filter
main = do
    compileFile stdin stdout
    

-- compile a single file (to core for now)
compileFile :: Handle -> Handle -> IO ()
compileFile input output = do
    s <- hGetContents input
    let (ParseOk hs) = parseModule s
    let b = toCore hs
    hPutStr output $ show b
    hPutStr output "\n"
    return ()