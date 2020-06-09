module Main where

import Parser
import Codegen
import Emit

import Control.Monad.Trans

import System.IO
import System.Environment
import System.Console.Haskeline

import qualified LLVM.AST as AST

initModule :: AST.Module
initModule = emptyModule "dsl2ir jit"

-- Given a String, try to compile it JIT style.
process :: AST.Module -> String -> IO (Maybe AST.Module)
process modo source = do
  let res = parseUpperLevel source
  case res of
    Left err -> print err >> return Nothing
    Right ex -> do
      ast <- codegen modo ex
      return $ Just ast

-- Trye to read a file and compile it.
processFile :: String -> IO (Maybe AST.Module)
processFile fname = readFile fname >>= process initModule

-- For our JIT, which will enter when we have no input files.
repl :: IO ()
repl = runInputT defaultSettings (loop initModule)
  where
  loop :: AST.Module -> InputT IO ()
  loop mod = do
    minput <- getInputLine "dsl2ir> "
    case minput of
      Nothing -> outputStrLn "See you later alligator."
      Just input -> do
        modn <- lift $ process mod input
        case modn of
          Just modn -> loop modn
          Nothing -> loop mod

main :: IO ()
main = do
  -- If we get an argument, assume it is a path to a file to process.
  args <- getArgs
  case args of
    []      -> repl
    [fname] -> processFile fname >> pure ()
