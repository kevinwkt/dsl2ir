module JIT where

import Data.Int
import Data.Word
import Foreign.Ptr ( FunPtr, castFunPtr )

import Control.Monad.Except

import LLVM.Target
import LLVM.Context
import LLVM.CodeModel
import LLVM.Module as Mod
import qualified LLVM.AST as AST

import LLVM.PassManager
import LLVM.Transforms
import LLVM.Analysis

import qualified LLVM.ExecutionEngine as EE

foreign import ccall "dynamic" haskFun :: FunPtr (IO Double) -> IO Double

run :: FunPtr a -> IO Double
run fnPtr = haskFun (castFunPtr fnPtr :: FunPtr (IO Double))

jit :: Context -> (EE.MCJIT -> IO a) -> IO a
jit c = EE.withMCJIT c oLevel model framePtrElim fastIns
  where
    oLevel = Just 0  -- optimization level
    model    = Nothing -- code model ( Default )
    framePtrElim  = Nothing -- frame pointer elimination
    fastIns  = Nothing -- fast instruction selection

astPasses :: PassSetSpec
astPasses = defaultCuratedPassSetSpec { optLevel = Just 3 }

-- Runs the JIT where it evaluates the given module.
runJIT :: AST.Module -> IO (Either String AST.Module)
runJIT mainModule = do
  withContext $ \context ->
    jit context $ \executionEngine ->
      runExceptT $ withModuleFromAST context mainModule $ \m ->
        withPassManager astPasses $ \pm -> do
          -- Optimization Pass
          runPassManager pm m
          optModule <- moduleAST m
          s <- moduleLLVMAssembly m
          putStrLn s

          EE.withModuleInEngine executionEngine m $ \ee -> do
            mainFn <- EE.getFunction ee (AST.Name "main")
            case mainFn of
              Just fn -> do
                ret <- run fn
                putStrLn $ "Evaluated to: " ++ show ret
              Nothing -> return ()

          -- Return the optimized module
          return optModule
