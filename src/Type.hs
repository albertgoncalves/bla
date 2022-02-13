module Type where

import Ast (AstExpr (..), AstPreFunc (..), AstStmt (..), AstType (..), Pos)
import Control.Monad (foldM)
import Data.Map (Map, fromList, insert, lookup, (!))
import Prelude hiding (lookup)

data Sig = Sig
  { getSigPos :: Pos,
    getSigArgs :: [AstType],
    getSigRet :: Maybe AstType
  }

getPos :: AstExpr -> Pos
getPos (AstExprInt pos _) = pos
getPos (AstExprVar pos _) = pos
getPos (AstExprUnOp pos _ _) = pos
getPos (AstExprBinOp pos _ _ _) = pos
getPos (AstExprCall pos _ _) = pos
getPos (AstExprAs pos _ _) = pos

typeError :: Pos -> (String, Pos)
typeError = (,) "type"

toType ::
  Map String Sig ->
  Map String AstType ->
  AstExpr ->
  Either (String, Pos) (Maybe AstType)
toType _ _ (AstExprInt pos _) = Right $ Just $ AstTypeI32 pos
toType sigs vars (AstExprAs _ expr type') = Just type' <$ toType sigs vars expr
toType sigs vars (AstExprVar pos ident) =
  case lookup ident vars of
    Just type' -> Right $ Just type'
    Nothing ->
      case lookup ident sigs of
        Just (Sig _ argTypes returnType) ->
          Right $ Just $ AstTypeFunc pos argTypes returnType
        Nothing -> Left $ typeError pos
toType sigs vars (AstExprUnOp pos _ expr) =
  case toType sigs vars expr of
    Right (Just (AstTypeI32 _)) -> Right $ Just $ AstTypeI32 pos
    Right _ -> Left $ typeError $ getPos expr
    l@(Left _) -> l
toType sigs vars (AstExprBinOp pos left _ right) =
  case toType sigs vars left of
    Right (Just (AstTypeI32 _)) ->
      case toType sigs vars right of
        Right (Just (AstTypeI32 _)) -> Right $ Just $ AstTypeI32 pos
        Right _ -> Left $ typeError $ getPos right
        l@(Left _) -> l
    Right _ -> Left $ typeError $ getPos left
    l@(Left _) -> l
toType sigs vars (AstExprCall _ expr args) = do
  case toType sigs vars expr of
    Right (Just (AstTypeFunc _ expectedArgTypes returnType)) ->
      case mapM (toType sigs vars) args of
        Right actualArgTypes ->
          if map Just expectedArgTypes == actualArgTypes
            then Right returnType
            else Left $ typeError $ getPos expr
        Left e -> Left e
    Right _ -> Left $ typeError $ getPos expr
    l@(Left _) -> l

checkStmt ::
  Map String Sig ->
  Int ->
  Maybe AstType ->
  Map String AstType ->
  AstStmt ->
  Either (String, Pos) (Map String AstType)
checkStmt sigs _ _ vars (AstStmtAssign _ ident expr) =
  case toType sigs vars expr of
    Right (Just rightType) ->
      case lookup ident vars of
        Just leftType ->
          if leftType == rightType
            then Right vars
            else Left $ typeError $ getPos expr
        Nothing -> Right $ insert ident rightType vars
    Right Nothing -> Left $ typeError $ getPos expr
    Left e -> Left e
checkStmt sigs depth returnType vars (AstStmtIf _ expr body) =
  case toType sigs vars expr of
    Right (Just (AstTypeI32 _)) ->
      foldM (checkStmt sigs depth returnType) vars body
    _ -> Left $ typeError $ getPos expr
checkStmt sigs depth returnType vars (AstStmtLoop _ body) =
  foldM (checkStmt sigs (depth + 1) returnType) vars body
checkStmt _ depth _ vars (AstStmtBreak pos n)
  | (0 <= n) && ((n + 1) <= depth) = Right vars
  | otherwise = Left $ typeError pos
checkStmt _ depth _ vars (AstStmtCont pos n)
  | (0 <= n) && ((n + 1) <= depth) = Right vars
  | otherwise = Left $ typeError pos
checkStmt sigs _ (Just returnType0) vars (AstStmtRet _ (Just expr)) =
  case toType sigs vars expr of
    Right (Just returnType1) ->
      if returnType0 == returnType1
        then Right vars
        else Left $ typeError $ getPos expr
    Right _ -> Left $ typeError $ getPos expr
    Left e -> Left e
checkStmt _ _ Nothing vars (AstStmtRet _ Nothing) = Right vars
checkStmt _ _ _ _ (AstStmtRet _ (Just expr)) = Left $ typeError $ getPos expr
checkStmt _ _ _ _ (AstStmtRet pos _) = Left $ typeError pos
checkStmt sigs _ _ vars (AstStmtDiscard _ expr) =
  case toType sigs vars expr of
    Right (Just _) -> Right vars
    Right _ -> Left $ typeError $ getPos expr
    Left e -> Left e
checkStmt sigs _ _ vars (AstStmtEffect _ expr) =
  case toType sigs vars expr of
    Right Nothing -> Right vars
    Right _ -> Left $ typeError $ getPos expr
    Left e -> Left e

checkFunc :: Map String Sig -> AstPreFunc -> Either (String, Pos) AstPreFunc
checkFunc sigs func =
  func
    <$ foldM
      (checkStmt sigs 0 $ getSigRet $ sigs ! getAstPreFuncName func)
      (fromList $ getAstPreFuncArgs func)
      (getAstPreFuncAst func)

toSig :: AstPreFunc -> (String, Sig)
toSig (AstPreFunc pos name args returnType _) =
  (name, Sig pos (map snd args) returnType)

checkFuncs :: [AstPreFunc] -> Either (String, Pos) [AstPreFunc]
checkFuncs funcs =
  case lookup "main" sigs of
    Just (Sig _ [] (Just (AstTypeI32 _))) -> mapM (checkFunc sigs) funcs
    Just (Sig pos _ _) -> Left $ typeError pos
    Nothing -> Left $ typeError 0
  where
    sigs = fromList $ map toSig funcs
