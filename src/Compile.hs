{-# LANGUAGE Strict #-}

module Compile where

import Ast (AstExpr (..), AstFunc (..), AstStmt (..), BinOp (..), UnOp (..))
import Data.Array (Array, listArray, (!))
import Data.List (foldl')
import qualified Data.Map.Strict as M
import Text.Printf (printf)

data Inst
  = InstHalt
  | InstPush
  | InstCopy
  | InstStore
  | InstDrop
  | InstRsrv
  | InstSwap
  | InstJump
  | InstJifz
  | InstAdd
  | InstSub
  | InstMul
  | InstDiv
  | InstEq
  | InstNeg
  | InstNot
  | InstLitInt Int
  | PreInstLabelSet String
  | PreInstLabelPush String
  deriving (Show)

newtype Node = Node {getNodeInt :: Int}

instance Show Node where
  show (Node x) = show x

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

store :: Int -> a -> [a] -> [a]
store _ _ [] = undefined
store 0 x xs = x : tail xs
store n _ xs
  | (n < 0) || (length xs < n) = undefined
store n x1 (x0 : xs) = x0 : store (n - 1) x1 xs

reserve :: Int -> a -> [a] -> [a]
reserve n a = (replicate n a ++)

swap :: [a] -> [a]
swap (b : a : xs) = a : b : xs
swap _ = undefined

pop1 :: [a] -> (a, [a])
pop1 (x : xs) = (x, xs)
pop1 _ = undefined

pop2 :: [a] -> (a, a, [a])
pop2 (b : a : xs) = (a, b, xs)
pop2 _ = undefined

lit :: Inst -> Int
lit (InstLitInt x) = x
lit _ = undefined

eval :: Array Int Inst -> Int -> [Node] -> [Node]
eval insts i xs =
  case insts ! i of
    InstHalt -> xs
    InstPush -> eval insts (i + 2) $ Node (lit $ insts ! (i + 1)) : xs
    InstCopy -> eval insts (i + 2) $ (xs !! lit (insts ! (i + 1))) : xs
    InstStore ->
      eval insts (i + 2) $ store (lit $ insts ! (i + 1)) (head xs) $ tail xs
    InstDrop -> eval insts (i + 2) $ drop (lit $ insts ! (i + 1)) xs
    InstRsrv ->
      eval insts (i + 2) $ reserve (lit $ insts ! (i + 1)) (Node 0) xs
    InstSwap -> eval insts (i + 1) $ swap xs
    InstJump -> eval insts (getNodeInt $ head xs) $ tail xs
    InstJifz ->
      let (a, b, xs') = pop2 xs
       in if getNodeInt b == 0
            then eval insts (getNodeInt a) xs'
            else eval insts (i + 1) xs'
    InstAdd ->
      let (a, b, xs') = pop2 xs
       in eval insts (i + 1) (Node (getNodeInt a + getNodeInt b) : xs')
    InstSub ->
      let (a, b, xs') = pop2 xs
       in eval insts (i + 1) (Node (getNodeInt a - getNodeInt b) : xs')
    InstMul ->
      let (a, b, xs') = pop2 xs
       in eval insts (i + 1) (Node (getNodeInt a * getNodeInt b) : xs')
    InstDiv ->
      let (a, b, xs') = pop2 xs
       in eval insts (i + 1) (Node (getNodeInt a `div` getNodeInt b) : xs')
    InstEq ->
      let (a, b, xs') = pop2 xs
       in eval
            insts
            (i + 1)
            (Node (if getNodeInt a == getNodeInt b then 1 else 0) : xs')
    InstNeg ->
      let (x, xs') = pop1 xs
       in eval insts (i + 1) (Node (negate (getNodeInt x)) : xs')
    InstNot ->
      let (x, xs') = pop1 xs
       in eval insts (i + 1) (Node (if getNodeInt x == 0 then 1 else 0) : xs')
    InstLitInt _ -> undefined
    PreInstLabelPush _ -> undefined
    PreInstLabelSet _ -> undefined

run :: [Inst] -> [Node]
run [] = undefined
run xs = eval (listArray (0, length xs - 1) xs) 0 []

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

popLabelLoop :: Context -> Int -> (LabelLoop, Context)
popLabelLoop
  (Context stackOffset vars (Labels labelRet (x : labelLoops)) compiler)
  n
    | n == 0 = (x, context)
    | n < 0 = undefined
    | otherwise = popLabelLoop context (n - 1)
    where
      context =
        Context stackOffset vars (Labels labelRet labelLoops) compiler
popLabelLoop _ _ = undefined

getVarOffset :: Context -> String -> Int
getVarOffset context name =
  getContextStackOffset context + getContextVars context M.! name

makeLabel :: Context -> String
makeLabel = printf "_%d" . getCompilerLabelCount . getContextCompiler

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
compileExpr context0 (AstExprCall _ name exprs) =
  appendContextInsts
    context1
    (pushVar context1 name ++ [InstJump, PreInstLabelSet label])
    `setStackOffset` succ (getContextStackOffset context0)
  where
    label = makeLabel context0
    context1 =
      foldl'
        compileExpr
        ( incrLabelCount $
            incrStackOffset $
              appendContextInsts context0 [PreInstLabelPush label]
        )
        exprs

compileStmt :: Context -> AstStmt -> Context
compileStmt context (AstStmtAssign _ name expr) =
  decrStackOffset $
    appendContextInsts
      (compileExpr context expr)
      [InstStore, InstLitInt $ getVarOffset context name]
compileStmt context0 (AstStmtIf _ condition body) =
  appendContextInsts context2 [PreInstLabelSet label]
  where
    label = makeLabel context0
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
compileStmt context (AstStmtRet _ expr) =
  decrStackOffset $
    appendContextInsts
      (compileExpr context expr)
      [ PreInstLabelPush $ getLabelRet $ getContextLabels context,
        InstJump
      ]
compileStmt context0 (AstStmtLoop _ body) =
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
    labelCont = makeLabel context0 ++ "_continue"
    labelBreak = makeLabel context1 ++ "_break"
compileStmt context0 (AstStmtBreak _ n) =
  appendContextInsts context1 [PreInstLabelPush labelBreak, InstJump]
  where
    (LabelLoop _ labelBreak, context1) = popLabelLoop context0 n
compileStmt context0 (AstStmtCont _ n) =
  appendContextInsts context1 [PreInstLabelPush labelCont, InstJump]
  where
    (LabelLoop labelCont _, context1) = popLabelLoop context0 n

appendCompilerInsts :: Compiler -> [Inst] -> Compiler
appendCompilerInsts (Compiler labelCount insts0) insts1 =
  Compiler labelCount $ insts0 ++ insts1

makeRetLabel :: String -> String
makeRetLabel = printf "_%s_return"

getVars :: [String] -> M.Map String Int
getVars xs = M.fromList $ zip (reverse xs) [0 ..]

compileFunc :: Compiler -> AstFunc -> Compiler
compileFunc compiler0 (AstFunc _ name [] [] body returnExpr) =
  appendCompilerInsts
    (getContextCompiler context2)
    [PreInstLabelSet returnLabel, InstSwap, InstJump]
  where
    returnLabel = makeRetLabel name
    context0 = Context 0 M.empty $ Labels returnLabel []
    context1 =
      foldl'
        compileStmt
        (context0 $ appendCompilerInsts compiler0 [PreInstLabelSet name])
        body
    context2 = compileExpr (context0 $ getContextCompiler context1) returnExpr
compileFunc compiler0 (AstFunc _ name args locals body returnExpr) =
  appendCompilerInsts (getContextCompiler context2) $
    PreInstLabelSet returnLabel :
    let n = length args + length locals - 1
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
  where
    returnLabel = makeRetLabel name
    context0 = Context 0 (getVars $ args ++ locals) $ Labels returnLabel []
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
    context2 = compileExpr (context0 $ getContextCompiler context1) returnExpr

compile :: [AstFunc] -> [Inst]
compile =
  getCompilerInsts
    . foldl'
      compileFunc
      ( Compiler
          1
          [ PreInstLabelPush "_0",
            PreInstLabelPush "main",
            InstJump,
            PreInstLabelSet "_0",
            InstHalt
          ]
      )

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
