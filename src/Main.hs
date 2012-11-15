
module Main where

import System.IO
import Text.Pretty
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
    let b  = toCore hs
    let ms = fromCore b
    
    hPutStr output $ show (pretty b)
    hPutStr output "\n"
    hPutStr output "\n"

    hPutStr output "----------------------------------------"
    hPutStr output "\n"
    hPutStr output "\n"

    hPutStr output $ show (pretty ms)
    hPutStr output "\n"
    
    return ()