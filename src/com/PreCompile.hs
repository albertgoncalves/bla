module PreCompile where

import Ast (AstFunc (..), AstPreFunc (..), AstStmt (..), Pos)
import Data.List (nub)

getPos :: AstStmt -> Pos
getPos (AstStmtAssign pos _ _) = pos
getPos (AstStmtIf pos _ _) = pos
getPos (AstStmtLoop pos _) = pos
getPos (AstStmtBreak pos _) = pos
getPos (AstStmtCont pos _) = pos
getPos (AstStmtRet pos _) = pos
getPos (AstStmtDiscard pos _) = pos
getPos (AstStmtEffect pos _) = pos
getPos (AstStmtSave pos _ _ _) = pos

getAssigns :: [AstStmt] -> [String]
getAssigns [] = []
getAssigns ((AstStmtAssign _ x _) : xs) = x : getAssigns xs
getAssigns ((AstStmtIf _ _ xs0) : xs1) = getAssigns $ xs0 ++ xs1
getAssigns ((AstStmtLoop _ xs0) : xs1) = getAssigns $ xs0 ++ xs1
getAssigns (_ : xs) = getAssigns xs

findUnreachable :: [AstStmt] -> [AstStmt]
findUnreachable [] = []
findUnreachable (AstStmtRet _ _ : xs) = xs
findUnreachable (AstStmtBreak _ _ : xs) = xs
findUnreachable (AstStmtCont _ _ : xs) = xs
findUnreachable (AstStmtIf _ _ xs0 : xs1) =
  case findUnreachable xs0 of
    [] -> findUnreachable xs1
    xs0' -> xs0'
findUnreachable (AstStmtLoop _ xs0 : xs1) =
  case findUnreachable xs0 of
    [] -> findUnreachable xs1
    xs0' -> xs0'
findUnreachable (_ : xs) = findUnreachable xs

walkBlock :: [AstStmt] -> Maybe ([AstStmt], AstStmt)
walkBlock [] = Nothing
walkBlock (AstStmtRet _ _ : x : _) = Just ([], x)
walkBlock [x@(AstStmtRet _ _)] = Just ([], x)
walkBlock (x0@(AstStmtIf _ _ xs0) : xs1) =
  case findUnreachable xs0 of
    [] -> do
      (xs1', x1) <- walkBlock xs1
      return (x0 : xs1', x1)
    (x : _) -> Just ([], x)
walkBlock (x0@(AstStmtLoop _ xs0) : xs1) =
  case findUnreachable xs0 of
    [] -> do
      (xs1', x1) <- walkBlock xs1
      return (x0 : xs1', x1)
    (x : _) -> Just ([], x)
walkBlock (x0 : xs) = do
  (xs', x1) <- walkBlock xs
  return (x0 : xs', x1)

intoFunc :: AstPreFunc -> Either Pos AstFunc
intoFunc (AstPreFunc pos name args _ body) =
  case walkBlock body of
    Nothing -> Left pos
    Just (body', AstStmtRet _ returnExpr) ->
      Right $
        AstFunc name (map fst args) (nub $ getAssigns body) body' returnExpr
    Just (_, x) -> Left $ getPos x

preCompile :: [AstPreFunc] -> Either (String, Pos) [AstFunc]
preCompile = either (Left . (,) "pre-compile") Right . mapM intoFunc
