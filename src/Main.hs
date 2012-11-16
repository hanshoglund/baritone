
module Main where

import Control.Monad (when)
import System.IO
import System.Exit
import System.Environment
import System.Console.GetOpt

import Text.Pretty

import Language.Haskell.Parser
import Language.Haskell.Syntax
import Language.Baritone

data BarOpt
    = Help    
    | Version
    | IncludeCore
    deriving (Eq, Show)

version = "baritone"
header  = "Usage: baritone [options] files...\n" ++
          "Options:"

options = [ 
    (Option ['h'] ["help"]          (NoArg Help)        "Print help and exit"),
    (Option ['v'] ["version"]       (NoArg Version)     "Print version and exit"),
    (Option []    ["include-core"]  (NoArg IncludeCore) "Include core in output")
  ]

usage = usageInfo header options
    
main = do
    (opts, args, optErrs) <- getOpt Permute options `fmap` getArgs

    when (Help `elem` opts) $ do
        putStr (usage ++ "\n")
        exitWith ExitSuccess

    when (Version `elem` opts) $ do
        putStr (version ++ "\n")
        exitWith ExitSuccess
  
    runFilter opts









runFilter opts = compileFile opts stdin stdout

compileFile :: [BarOpt] -> Handle -> Handle -> IO ()
compileFile opts input output = do
    s <- hGetContents input
    let hs = parse s
    let b  = transform (singleApp . singleAbs) $ fromHaskell hs
    let ms = toManuScript b
    
    when (IncludeCore `elem` opts) $ do
        hPutStr output $ show (pretty b)
        hPutStr output "\n"
        hPutStr output "\n"
    
    hPutStr output $ show (pretty ms)
    hPutStr output "\n"
    
    return ()

    where
        parse :: String -> HsModule
        parse s = case (parseModule s) of
            ParseOk hs -> hs
            ParseFailed (SrcLoc f l c) e -> error $ f ++ ":" ++ show l ++ ":" ++ show c ++ ": " ++ e
