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
  | AstExprCall Pos AstExpr [AstExpr]
  | AstExprAs Pos AstExpr AstType

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

instance Eq AstType where
  (AstTypeI32 _) == (AstTypeI32 _) = True
  (AstTypeFunc _ as0 r0) == (AstTypeFunc _ as1 r1) = (as0 == as1) && (r0 == r1)
  _ == _ = False

data AstPreFunc = AstPreFunc
  { getAstPreFuncPos :: Pos,
    getAstPreFuncName :: String,
    getAstPreFuncArgs :: [(String, AstType)],
    getAstPreFuncRetType :: Maybe AstType,
    getAstPreFuncAst :: [AstStmt]
  }

data AstFunc = AstFunc
  { getAstFuncName :: String,
    getAstFuncArgs :: [String],
    getAstFuncLocals :: [String],
    getAstFuncAst :: [AstStmt],
    getAstFuncRet :: Maybe AstExpr
  }
