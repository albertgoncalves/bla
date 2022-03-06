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
getPos (AstExprRead pos _ _) = pos

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
toType sigs vars (AstExprUnOp pos _ expr) = do
  r <- toType sigs vars expr
  case r of
    (Just (AstTypeI32 _)) -> Right $ Just $ AstTypeI32 pos
    _ -> Left $ typeError $ getPos expr
toType sigs vars (AstExprBinOp pos left _ right) = do
  r0 <- toType sigs vars left
  case r0 of
    (Just (AstTypeI32 _)) -> do
      r1 <- toType sigs vars right
      case r1 of
        (Just (AstTypeI32 _)) -> Right $ Just $ AstTypeI32 pos
        _ -> Left $ typeError $ getPos right
    _ -> Left $ typeError $ getPos left
toType sigs vars (AstExprCall _ expr args) = do
  r <- toType sigs vars expr
  case r of
    (Just (AstTypeFunc _ expectedArgTypes returnType)) -> do
      actualArgTypes <- mapM (toType sigs vars) args
      if map Just expectedArgTypes == actualArgTypes
        then Right returnType
        else Left $ typeError $ getPos expr
    _ -> Left $ typeError $ getPos expr
toType sigs vars (AstExprRead pos base offset) = do
  r0 <- toType sigs vars base
  case r0 of
    (Just (AstTypeAddr _)) -> do
      r1 <- toType sigs vars offset
      case r1 of
        (Just (AstTypeI32 _)) -> Right $ Just $ AstTypeI32 pos
        _ -> Left $ typeError $ getPos offset
    _ -> Left $ typeError $ getPos base

checkStmt ::
  Map String Sig ->
  Int ->
  Maybe AstType ->
  Map String AstType ->
  AstStmt ->
  Either (String, Pos) (Map String AstType)
checkStmt sigs _ _ vars (AstStmtAssign _ ident expr) = do
  r <- toType sigs vars expr
  case r of
    (Just rightType) ->
      case lookup ident vars of
        Just leftType ->
          if leftType == rightType
            then Right vars
            else Left $ typeError $ getPos expr
        Nothing -> Right $ insert ident rightType vars
    Nothing -> Left $ typeError $ getPos expr
checkStmt sigs depth returnType vars (AstStmtIf _ expr body) = do
  r <- toType sigs vars expr
  case r of
    (Just (AstTypeI32 _)) ->
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
checkStmt sigs _ (Just returnType0) vars (AstStmtRet _ (Just expr)) = do
  r <- toType sigs vars expr
  case r of
    (Just returnType1) ->
      if returnType0 == returnType1
        then Right vars
        else Left $ typeError $ getPos expr
    _ -> Left $ typeError $ getPos expr
checkStmt _ _ Nothing vars (AstStmtRet _ Nothing) = Right vars
checkStmt _ _ _ _ (AstStmtRet _ (Just expr)) = Left $ typeError $ getPos expr
checkStmt _ _ _ _ (AstStmtRet pos _) = Left $ typeError pos
checkStmt sigs _ _ vars (AstStmtDiscard _ expr) = do
  r <- toType sigs vars expr
  case r of
    (Just _) -> Right vars
    _ -> Left $ typeError $ getPos expr
checkStmt sigs _ _ vars (AstStmtEffect _ expr) = do
  r <- toType sigs vars expr
  case r of
    Nothing -> Right vars
    _ -> Left $ typeError $ getPos expr
checkStmt sigs _ _ vars (AstStmtSave _ base offset expr) = do
  r0 <- toType sigs vars base
  case r0 of
    Just (AstTypeAddr _) -> do
      r1 <- toType sigs vars offset
      case r1 of
        Just (AstTypeI32 _) -> do
          r2 <- toType sigs vars expr
          case r2 of
            Just (AstTypeI32 _) -> Right vars
            _ -> Left $ typeError $ getPos expr
        _ -> Left $ typeError $ getPos offset
    _ -> Left $ typeError $ getPos base

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

intrinsics :: [(String, Sig)]
intrinsics =
  [ ("@alloc_heap", Sig 0 [AstTypeI32 0] $ Just $ AstTypeAddr 0),
    ("@print_char", Sig 0 [AstTypeI32 0] Nothing),
    ("@print_i32", Sig 0 [AstTypeI32 0] Nothing)
  ]

checkFuncs :: [AstPreFunc] -> Either (String, Pos) [AstPreFunc]
checkFuncs funcs =
  case lookup "main" sigs of
    Just (Sig _ [] Nothing) -> mapM (checkFunc sigs) funcs
    Just (Sig pos _ _) -> Left $ typeError pos
    Nothing -> Left $ typeError 0
  where
    sigs = fromList $ intrinsics ++ map toSig funcs
