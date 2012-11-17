
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
    | JustCore
    deriving (Eq, Show)

version = "baritone"
header  = "Usage: baritone [options] files...\n" ++
          "Options:"

options = [ 
    (Option ['h'] ["help"]          (NoArg Help)        "Print help and exit"),
    (Option ['v'] ["version"]       (NoArg Version)     "Print version and exit"),
    (Option []    ["include-core"]  (NoArg IncludeCore) "Include core in output"),
    (Option []    ["output-core"]   (NoArg JustCore)    "Output core only")
  ]
    
main = do
    (opts, args, optErrs) <- getOpt Permute options `fmap` getArgs

    let usage = usageInfo header options
    let printUsage   = putStr (usage ++ "\n") >> exitWith ExitSuccess
    let printVersion = putStr (version ++ "\n") >> exitWith ExitSuccess

    when (Help `elem` opts) printUsage
    when (Version `elem` opts) printVersion  
    runFilter opts


-- |
-- Run as a filter from stdin to stdout.
runFilter :: [BarOpt] -> IO ()
runFilter opts = compileFile opts stdin stdout

compileFile :: [BarOpt] -> Handle -> Handle -> IO ()
compileFile opts input output = do
    s <- hGetContents input
                         
    let coreToCore = singleAbs . singleApp
    let hs = parse s
    let b  = fromHaskell hs
    let b' = transform coreToCore b
    let ms = toManuScript b'
    
    when (IncludeCore `elem` opts || JustCore `elem` opts) $ do
        hPutStr output $ show (pretty b')
        hPutStr output "\n"
        hPutStr output "\n"

    when (not $ JustCore `elem` opts) $ do
        hPutStr output $ show (pretty ms)
        hPutStr output "\n"
        hPutStr output "\n"
    
    return ()

    where
        parse :: String -> HsModule
        parse s = case (parseModule s) of
            ParseOk hs -> hs
            ParseFailed (SrcLoc f l c) e -> error $ f ++ ":" ++ show l ++ ":" ++ show c ++ ": " ++ e
