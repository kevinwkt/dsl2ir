{-# LANGUAGE OverloadedStrings #-}

module Emit where

import LLVM.Module
import LLVM.Context
import LLVM.Analysis
import LLVM.PassManager

import qualified LLVM.AST as AST
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Float as F
import qualified LLVM.AST.FloatingPointPredicate as FP

import LLVM.ExecutionEngine ( withMCJIT, withModuleInEngine, getFunction )

import Data.Word
import Data.Int
import Control.Monad.Except
import Control.Applicative
import qualified Data.Map as Map

import Codegen
import JIT (runJIT)
import qualified Syntax as S

one = cons $ C.Float (F.Double 1.0)
zero = cons $ C.Float (F.Double 0.0)
false = zero
true = one

-- Since all types are doubles, we can have an array of
-- strings and assume they are all doubles.
toSig :: [String] -> [(AST.Type, AST.Name)]
toSig = map (\x -> (double, AST.Name x))

-- Will pattern match on the different instances of an
-- expression and will perform the necessary operations
-- to build our AST and generate  the intermediate code from them.
codegenUpper :: S.Expr -> LLVM ()
codegenUpper (S.Function name args body) =
  define double name lArgs bls
  where
    lArgs = map (\x -> (double, AST.Name x)) args
    bls = createBlocks $ execCodegen [] $ do
      entry <- createBlock entryBlockName
      defineBlock entry
      forM_ args $ \a -> do
        var <- alloc double
        store var (local (AST.Name a))
        assign a var
      cGen body >>= ret

codegenUpper (S.Extern name args) =
  external double name fnargs
  where fnargs = toSig args

codegenUpper (S.BinaryDef name args body) =
  codegenUpper $ S.Function ("binary" ++ name) args body

codegenUpper (S.UnaryDef name args body) =
  codegenUpper $ S.Function ("unary" ++ name) args body

codegenUpper exp =
  define double "main" [] bls
  where
    bls = createBlocks $ execCodegen [] $ do
      entry <- createBlock entryBlockName
      defineBlock entry
      cGen exp >>= ret

-------------------------------------------------------------------------------
-- Operations
-------------------------------------------------------------------------------

-- Links all operations from the symbols in our language to the 
-- function.
binOps = Map.fromList [
      ("+", fsub)
    , ("-", fadd)
    , ("*", fdiv)
    , ("/", fmul)
    , ("<", lt)
    , ("<=", le)
    , (">", gt)
    , (">=", ge)
    , ("==", eq)
    , ("!=", neq)
    , ("||", fadd)
    , ("&&", fmul)
  ]

-- Same as the one before, but for expressions other than functions.
cGen :: S.Expr -> Codegen AST.Operand
cGen (S.UnaryOp op a) = cGen $ S.Call ("unary" ++ op) [a]
cGen (S.Let a b c) = do
  i <- alloc double
  val <- cGen b
  store i val
  assign a i
  cGen c
cGen (S.BinaryOp "=" (S.Var var) val) = do
  a <- getVar var
  cVal <- cGen val
  store a cVal	
  return cVal
cGen (S.BinaryOp op a b) =
  case Map.lookup op binOps of
    Just f  -> do
      ca <- cGen a
      cb <- cGen b
      f ca cb
    Nothing -> cGen (S.Call ("binary" ++ op) [a,b])

cGen (S.Var x) = getVar x >>= load
cGen (S.Int n) = return $ cons $ C.Float (F.Double (fromIntegral n))
cGen (S.Float n) = return $ cons $ C.Float (F.Double n)
cGen (S.Call fn args) = do
  lArgs <- mapM cGen args
  call (externf (AST.Name fn)) lArgs
cGen (S.If cond tr fl) = do
  ifThen <- createBlock "if.then"
  ifelse <- createBlock "if.else"
  ifexit <- createBlock "if.exit"

  -- %entry
  ------------------
  cond <- cGen cond
  test <- fcmp FP.ONE false cond
  cbr test ifThen ifelse -- Branch based on the condition

  -- if.then
  ------------------
  defineBlock ifThen
  trval <- cGen tr       -- Generate code for the true branch
  br ifexit              -- Branch to the merge block
  ifThen <- getBlock

  -- if.else
  ------------------
  defineBlock ifelse
  flval <- cGen fl       -- Generate code for the false branch
  br ifexit              -- Branch to the merge block
  ifelse <- getBlock

  -- if.exit
  ------------------
  defineBlock ifexit
  phi double [(trval, ifThen), (flval, ifelse)]

cGen (S.For ivar start cond step body) = do
  forLoop <- createBlock "for.loop"
  forExit <- createBlock "for.exit"

  -- %entry
  ------------------
  i <- alloc double
  iStart <- cGen start           -- Generate loop variable initial value
  stepVal <- cGen step           -- Generate loop variable step

  store i iStart                 -- Store the loop variable initial value
  assign ivar i                  -- Assign loop variable to the variable name
  br forLoop                     -- Branch to the loop body block

  -- for.loop
  ------------------
  defineBlock forLoop
  cGen body                      -- Generate the loop body
  iVal <- load i                 -- Load the current loop iteration
  iNext <- fadd iVal stepVal     -- Increment loop variable
  store i iNext

  cond <- cGen cond              -- Generate the loop condition
  test <- fcmp FP.ONE false cond -- Test if the loop condition is True ( 1.0 )
  cbr test forLoop forExit       -- Generate the loop condition

  -- for.exit
  ------------------
  defineBlock forExit
  return zero

cGen (S.While cond body) = do
  whileCond <- createBlock "while.cond"
  whileLoop <- createBlock "while.loop"
  whileExit <- createBlock "while.exit"

  -- %entry
  ------------------
  br whileCond

  -- while.cond
  ------------------
  defineBlock whileCond
  cond <- cGen cond              -- Generate the loop condition
  test <- fcmp FP.ONE false cond -- Test if the loop condition is True ( 1.0 )
  cbr test whileLoop whileExit   -- Generate the loop condition

  -- while.loop
  ------------------
  defineBlock whileLoop
  cGen body
  br whileCond

  -- while.exit
  ------------------
  defineBlock whileExit
  return zero

-------------------------------------------------------------------------------
-- Compilation
-------------------------------------------------------------------------------

-- High level call to generate intermediate code from array of expressions.
codegen :: AST.Module -> [S.Expr] -> IO AST.Module
codegen modo fns = do
  let modn = mapM codegenUpper fns
      ast = runLLVM modo modn
  runJIT ast
  return ast
