module Compile where

import Ast (AstExpr (..), AstFunc (..), AstStmt (..), BinOp (..), UnOp (..))
import Data.List (foldl')
import qualified Data.Map.Strict as M
import Text.Printf (printf)
import Vm (Inst (..))

data LabelLoop = LabelLoop
  { getLabelCont :: String,
    getLabelBreak :: String
  }

data Labels = Labels
  { getLabelRet :: String,
    getLabelLoop :: [LabelLoop]
  }

data Context = Context
  { getContextStackOffset :: Int,
    getContextVars :: M.Map String Int,
    getContextLabels :: Labels,
    getContextCompiler :: Compiler
  }

data Compiler = Compiler
  { getCompilerLabelCount :: Int,
    getCompilerInsts :: [Inst]
  }

unOpToInst :: UnOp -> Inst
unOpToInst UnOpMinus = InstNeg
unOpToInst UnOpBang = InstNot

binOpToInst :: BinOp -> Inst
binOpToInst BinOpAdd = InstAdd
binOpToInst BinOpSub = InstSub
binOpToInst BinOpMul = InstMul
binOpToInst BinOpDiv = InstDiv
binOpToInst BinOpEq = InstEq

incrStackOffset :: Context -> Context
incrStackOffset (Context stackOffset vars labels compiler) =
  Context (succ stackOffset) vars labels compiler

decrStackOffset :: Context -> Context
decrStackOffset (Context stackOffset vars labels compiler) =
  Context (pred stackOffset) vars labels compiler

setStackOffset :: Context -> Int -> Context
setStackOffset (Context _ vars labels compiler) stackOffset =
  Context stackOffset vars labels compiler

incrLabelCount :: Context -> Context
incrLabelCount (Context stackOffset vars labels (Compiler labelCount insts)) =
  Context stackOffset vars labels $ Compiler (succ labelCount) insts

appendContextInsts :: Context -> [Inst] -> Context
appendContextInsts (Context stackOffset vars labels compiler) insts =
  Context stackOffset vars labels $ appendCompilerInsts compiler insts

pushLabelLoop :: Context -> LabelLoop -> Context
pushLabelLoop
  (Context stackOffset vars (Labels labelRet labelLoops) compiler)
  x =
    Context
      stackOffset
      vars
      (Labels labelRet $ x : labelLoops)
      compiler

findLabelLoop :: Int -> Context -> LabelLoop
findLabelLoop n = (!! n) . getLabelLoop . getContextLabels

dropLabelLoop :: Context -> Context
dropLabelLoop
  (Context stackOffset vars (Labels labelRet (_ : labelLoops)) compiler) =
    Context stackOffset vars (Labels labelRet labelLoops) compiler
dropLabelLoop _ = undefined

getVarOffset :: Context -> String -> Int
getVarOffset context name =
  getContextStackOffset context + getContextVars context M.! name

getLabel :: Int -> String
getLabel = printf "_%d"

labelFromContext :: Context -> String
labelFromContext = getLabel . getCompilerLabelCount . getContextCompiler

pushVar :: Context -> String -> [Inst]
pushVar context name =
  if M.member name $ getContextVars context
    then [InstCopy, InstLitInt $ getVarOffset context name]
    else [PreInstLabelPush name]

compileExpr :: Context -> AstExpr -> Context
compileExpr context (AstExprInt _ x) =
  incrStackOffset $ appendContextInsts context [InstPush, InstLitInt x]
compileExpr context (AstExprVar _ name) =
  incrStackOffset $ appendContextInsts context $ pushVar context name
compileExpr context (AstExprUnOp _ op expr) =
  appendContextInsts (compileExpr context expr) [unOpToInst op]
compileExpr context (AstExprBinOp _ l op r) =
  decrStackOffset $
    appendContextInsts (compileExpr (compileExpr context l) r) [binOpToInst op]
-- NOTE: Even though some functions do not return values, we always augment
-- the stack here _as if_ that was true. We end up handling this when compiling
-- `AstStmtEffect` statements below, which blindly decrements the stack offset.
compileExpr context0 (AstExprCall _ expr args) =
  appendContextInsts context2 [InstJump, PreInstLabelSet label]
    `setStackOffset` succ (getContextStackOffset context0)
  where
    label = labelFromContext context0
    context1 =
      foldl'
        compileExpr
        ( incrLabelCount $
            incrStackOffset $
              appendContextInsts context0 [PreInstLabelPush label]
        )
        args
    context2 = compileExpr context1 expr

compileStmt :: Context -> AstStmt -> Context
compileStmt context (AstStmtAssign _ name expr) =
  decrStackOffset $
    appendContextInsts
      (compileExpr context expr)
      [InstStore, InstLitInt $ getVarOffset context name]
compileStmt context0 (AstStmtIf _ condition body) =
  appendContextInsts context2 [PreInstLabelSet label]
  where
    label = labelFromContext context0
    context1 =
      compileExpr
        ( appendContextInsts
            (incrLabelCount $ incrStackOffset context0)
            [PreInstLabelPush label]
        )
        condition
    context2 =
      foldl'
        compileStmt
        ( appendContextInsts
            context1
            [InstJifz]
            `setStackOffset` getContextStackOffset context0
        )
        body
compileStmt context (AstStmtRet _ (Just expr)) =
  decrStackOffset $
    appendContextInsts
      (compileExpr context expr)
      [ PreInstLabelPush $ getLabelRet $ getContextLabels context,
        InstJump
      ]
compileStmt context (AstStmtRet _ Nothing) =
  decrStackOffset $
    appendContextInsts
      context
      [ PreInstLabelPush $ getLabelRet $ getContextLabels context,
        InstJump
      ]
compileStmt context0 (AstStmtLoop _ body) =
  dropLabelLoop $
    appendContextInsts
      ( foldl'
          compileStmt
          (appendContextInsts context2 [PreInstLabelSet labelCont])
          body
      )
      [PreInstLabelPush labelCont, InstJump, PreInstLabelSet labelBreak]
  where
    context1 = incrLabelCount context0
    context2 =
      incrLabelCount $
        pushLabelLoop context1 $
          LabelLoop labelCont labelBreak
    labelCont = labelFromContext context0 ++ "_continue"
    labelBreak = labelFromContext context1 ++ "_break"
compileStmt context (AstStmtBreak _ n) =
  appendContextInsts context [PreInstLabelPush labelBreak, InstJump]
  where
    (LabelLoop _ labelBreak) = findLabelLoop n context
compileStmt context (AstStmtCont _ n) =
  appendContextInsts context [PreInstLabelPush labelCont, InstJump]
  where
    (LabelLoop labelCont _) = findLabelLoop n context
compileStmt context (AstStmtDiscard _ expr) =
  decrStackOffset $
    appendContextInsts (compileExpr context expr) [InstDrop, InstLitInt 1]
-- NOTE: We need to decrement the stack offset here because the compiler always
-- thinks a function call will leave a value on the stack. It may be nicer to
-- compile function calls in a more thoughtful way; one in which functions that
-- do not return a value do not result in stack offset being incremented. Doing
-- this would require augmenting the `Ast` information the compiler has access
-- to. This _seems_ to work for now, but I'm worried there is a bug waiting to
-- be found here.
compileStmt context (AstStmtEffect _ expr) =
  decrStackOffset $ compileExpr context expr

appendCompilerInsts :: Compiler -> [Inst] -> Compiler
appendCompilerInsts (Compiler labelCount insts0) insts1 =
  Compiler labelCount $ insts0 ++ insts1

makeRetLabel :: String -> String
makeRetLabel = printf "_%s_return"

getVars :: [String] -> M.Map String Int
getVars xs = M.fromList $ zip (reverse xs) [0 ..]

compileFunc :: Compiler -> AstFunc -> Compiler
compileFunc compiler0 (AstFunc _ name [] [] body returnExpr) =
  appendCompilerInsts (getContextCompiler context2) $
    PreInstLabelSet returnLabel : case returnExpr of
      Just _ -> [InstSwap, InstJump]
      Nothing -> [InstJump]
  where
    returnLabel = makeRetLabel name
    context0 = Context 0 M.empty $ Labels returnLabel []
    context1 =
      foldl'
        compileStmt
        (context0 $ appendCompilerInsts compiler0 [PreInstLabelSet name])
        body
    context2 =
      maybe
        (context0 $ getContextCompiler context1)
        (compileExpr $ context0 $ getContextCompiler context1)
        (fst <$> returnExpr)
compileFunc compiler0 (AstFunc _ name args locals body returnExpr) =
  appendCompilerInsts (getContextCompiler context2) $
    case returnExpr of
      Just _ ->
        PreInstLabelSet returnLabel :
        let n = length args' + length locals - 1
         in if n == 0
              then [InstStore, InstLitInt n, InstSwap, InstJump]
              else
                [ InstStore,
                  InstLitInt n,
                  InstDrop,
                  InstLitInt n,
                  InstSwap,
                  InstJump
                ]
      Nothing ->
        let n = length args' + length locals
         in [PreInstLabelSet returnLabel, InstDrop, InstLitInt n, InstJump]
  where
    args' = map fst args
    returnLabel = makeRetLabel name
    context0 = Context 0 (getVars $ args' ++ locals) $ Labels returnLabel []
    context1 =
      foldl'
        compileStmt
        ( context0 $
            appendCompilerInsts compiler0 $
              PreInstLabelSet name :
              let n = length locals
               in if n == 0
                    then []
                    else [InstRsrv, InstLitInt n]
        )
        body
    context2 =
      maybe
        (context0 $ getContextCompiler context1)
        (compileExpr $ context0 $ getContextCompiler context1)
        (fst <$> returnExpr)

entryPoint :: String -> Int -> (Int, [Inst])
entryPoint name n =
  ( succ n,
    [ PreInstLabelPush label,
      PreInstLabelPush name,
      InstJump,
      PreInstLabelSet label,
      InstHalt
    ]
  )
  where
    label = getLabel n

compile :: [AstFunc] -> [Inst]
compile =
  getCompilerInsts
    . foldl' compileFunc (uncurry Compiler $ entryPoint "main" 0)

weightInst :: Inst -> Int
weightInst (PreInstLabelPush _) = 2
weightInst (PreInstLabelSet _) = 0
weightInst _ = 1

getLabels :: [Inst] -> M.Map String Int
getLabels xs = M.fromList $ f $ zip xs $ scanl (+) 0 $ map weightInst xs
  where
    f [] = []
    f ((PreInstLabelSet x, n) : xs') = (x, n) : f xs'
    f (_ : xs') = f xs'

resolve :: [Inst] -> M.Map String Int -> [Inst]
resolve [] _ = []
resolve (PreInstLabelSet _ : xs) m = resolve xs m
resolve (PreInstLabelPush x : xs) m =
  [InstPush, InstLitInt (m M.! x)] ++ resolve xs m
resolve (x : xs) m = x : resolve xs m

assemble :: [Inst] -> [Inst]
assemble xs = resolve xs $ getLabels xs
