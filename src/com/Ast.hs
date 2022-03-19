module Ast where

data UnOp
  = UnOpBang
  | UnOpMinus

data BinOp
  = BinOpAdd
  | BinOpSub
  | BinOpMul
  | BinOpDiv
  | BinOpAnd
  | BinOpOr
  | BinOpShl
  | BinOpShr
  | BinOpEq

type Pos = Int

data AstExpr
  = AstExprInt Pos Int
  | AstExprVar Pos String
  | AstExprUnOp Pos UnOp AstExpr
  | AstExprBinOp Pos AstExpr BinOp AstExpr
  | AstExprCall Pos AstExpr [AstExpr]
  | AstExprAs Pos AstExpr AstType
  | AstExprRead Pos AstExpr AstExpr

data AstStmt
  = AstStmtAssign Pos String AstExpr
  | AstStmtIf Pos AstExpr [AstStmt]
  | AstStmtLoop Pos [AstStmt]
  | AstStmtBreak Pos Int
  | AstStmtCont Pos Int
  | AstStmtRet Pos (Maybe AstExpr)
  | AstStmtDiscard Pos AstExpr
  | AstStmtEffect Pos AstExpr
  | AstStmtSave Pos AstExpr AstExpr AstExpr

data AstType
  = AstTypeI32 Pos
  | AstTypeAddr Pos
  | AstTypeFunc Pos [AstType] (Maybe AstType)

instance Eq AstType where
  (AstTypeI32 _) == (AstTypeI32 _) = True
  (AstTypeAddr _) == (AstTypeAddr _) = True
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
