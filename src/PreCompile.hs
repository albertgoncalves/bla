module PreCompile where

import Ast (AstFunc (..), AstPreFunc (..), AstStmt (..), Pos)
import Data.List (nub)

popLast :: [a] -> Maybe ([a], a)
popLast xs =
  case reverse xs of
    [] -> Nothing
    (x : xs') -> Just (reverse xs', x)

getStmtPos :: AstStmt -> Pos
getStmtPos (AstStmtAssign pos _ _) = pos
getStmtPos (AstStmtIf pos _ _) = pos
getStmtPos (AstStmtLoop pos _) = pos
getStmtPos (AstStmtBreak pos _) = pos
getStmtPos (AstStmtCont pos _) = pos
getStmtPos (AstStmtRet pos _) = pos

collectAssigns :: [AstStmt] -> [String]
collectAssigns [] = []
collectAssigns ((AstStmtAssign _ x _) : xs) = x : collectAssigns xs
collectAssigns ((AstStmtIf _ _ xs0) : xs1) = collectAssigns $ xs0 ++ xs1
collectAssigns ((AstStmtLoop _ xs0) : xs1) = collectAssigns $ xs0 ++ xs1
collectAssigns (_ : xs) = collectAssigns xs

intoFunc :: AstPreFunc -> Either Pos AstFunc
intoFunc (AstPreFunc pos name args body) =
  case popLast body of
    Nothing -> Left pos
    Just (body', AstStmtRet _ returnExpr) ->
      Right $
        AstFunc pos name args (nub $ collectAssigns body) body' returnExpr
    Just (_, x) -> Left $ getStmtPos x

preCompile :: [AstPreFunc] -> Either (String, Pos) [AstFunc]
preCompile = either (Left . (,) "pre-compile") Right . mapM intoFunc
