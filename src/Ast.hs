module Ast where

data UnOp
  = UnOpBang
  | UnOpMinus
  deriving (Show)

data BinOp
  = BinOpAdd
  | BinOpSub
  | BinOpMul
  | BinOpDiv
  | BinOpEq
  deriving (Show)

type Pos = Int

data AstExpr
  = AstExprInt Pos Int
  | AstExprVar Pos String
  | AstExprUnOp Pos UnOp AstExpr
  | AstExprBinOp Pos AstExpr BinOp AstExpr
  | AstExprCall Pos String [AstExpr]
  deriving (Show)

data AstStmt
  = AstStmtAssign Pos String AstExpr
  | AstStmtIf Pos AstExpr [AstStmt]
  | AstStmtLoop Pos [AstStmt]
  | AstStmtBreak Pos Int
  | AstStmtCont Pos Int
  | AstStmtRet Pos AstExpr
  deriving (Show)

data AstPreFunc = AstPreFunc
  { getAstPreFuncPos :: Pos,
    getAstPreFuncName :: String,
    getAstPreFuncArgs :: [String],
    getAstPreFuncAst :: [AstStmt]
  }
  deriving (Show)

data AstFunc = AstFunc
  { getAstFuncPos :: Pos,
    getAstFuncName :: String,
    getAstFuncArgs :: [String],
    getAstFuncLocals :: [String],
    getAstFuncAst :: [AstStmt],
    getAstFuncRet :: AstExpr
  }
  deriving (Show)
