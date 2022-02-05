module PreCompile where

import Ast (AstFunc (..), AstPreFunc (..), AstStmt (..), Pos)
import Data.List (nub)

popLast :: [a] -> Maybe ([a], a)
popLast xs =
  case reverse xs of
    [] -> Nothing
    (x : xs') -> Just (reverse xs', x)

getPos :: AstStmt -> Pos
getPos (AstStmtAssign pos _ _) = pos
getPos (AstStmtIf pos _ _) = pos
getPos (AstStmtLoop pos _) = pos
getPos (AstStmtBreak pos _) = pos
getPos (AstStmtCont pos _) = pos
getPos (AstStmtRet pos _) = pos
getPos (AstStmtDiscard pos _) = pos
getPos (AstStmtEffect pos _) = pos

getAssigns :: [AstStmt] -> [String]
getAssigns [] = []
getAssigns ((AstStmtAssign _ x _) : xs) = x : getAssigns xs
getAssigns ((AstStmtIf _ _ xs0) : xs1) = getAssigns $ xs0 ++ xs1
getAssigns ((AstStmtLoop _ xs0) : xs1) = getAssigns $ xs0 ++ xs1
getAssigns (_ : xs) = getAssigns xs

intoFunc :: AstPreFunc -> Either Pos AstFunc
intoFunc (AstPreFunc pos name args body) =
  case popLast body of
    Nothing -> Left pos
    Just (body', AstStmtRet _ returnExpr) ->
      Right $ AstFunc pos name args (nub $ getAssigns body) body' returnExpr
    Just (_, x) -> Left $ getPos x

preCompile :: [AstPreFunc] -> Either (String, Pos) [AstFunc]
preCompile = either (Left . (,) "pre-compile") Right . mapM intoFunc
