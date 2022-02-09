module Ast where

data UnOp
  = UnOpBang
  | UnOpMinus

data BinOp
  = BinOpAdd
  | BinOpSub
  | BinOpMul
  | BinOpDiv
  | BinOpEq

type Pos = Int

data AstExpr
  = AstExprInt Pos Int
  | AstExprVar Pos String
  | AstExprUnOp Pos UnOp AstExpr
  | AstExprBinOp Pos AstExpr BinOp AstExpr
  | AstExprCall Pos String [AstExpr]

data AstStmt
  = AstStmtAssign Pos String AstExpr
  | AstStmtIf Pos AstExpr [AstStmt]
  | AstStmtLoop Pos [AstStmt]
  | AstStmtBreak Pos Int
  | AstStmtCont Pos Int
  | AstStmtRet Pos (Maybe AstExpr)
  | AstStmtDiscard Pos AstExpr
  | AstStmtEffect Pos AstExpr

data AstType
  = AstTypeI32 Pos
  | AstTypeFunc Pos [AstType] (Maybe AstType)
  deriving (Eq)

data AstPreFunc = AstPreFunc
  { getAstPreFuncPos :: Pos,
    getAstPreFuncName :: String,
    getAstPreFuncArgs :: [(String, AstType)],
    getAstPreFuncRetType :: Maybe AstType,
    getAstPreFuncAst :: [AstStmt]
  }

data AstFunc = AstFunc
  { getAstFuncPos :: Pos,
    getAstFuncName :: String,
    getAstFuncArgs :: [(String, AstType)],
    getAstFuncLocals :: [String],
    getAstFuncAst :: [AstStmt],
    getAstFuncRet :: Maybe (AstExpr, AstType)
  }
