module Type where

import Ast (AstExpr (..), AstFunc (..), AstStmt (..), AstType (..), Pos)
import Control.Monad (foldM)
import Data.Map (Map, empty, fromList, lookup)
import Data.Maybe (catMaybes)
import Prelude hiding (lookup)

data Sig = Sig
  { getSigPos :: Pos,
    getSigArgs :: [AstType],
    getSigRet :: Maybe AstType
  }

typeError :: Pos -> (String, Pos)
typeError = (,) "type"

toSig :: AstFunc -> (String, Sig)
toSig (AstFunc pos name args _ _ return') =
  (name, Sig pos (snd <$> args) (snd <$> return'))

findCallType ::
  Map String Sig ->
  Map String AstType ->
  AstExpr ->
  Maybe AstType
findCallType = undefined

toType ::
  Map String Sig ->
  Map String AstType ->
  AstExpr ->
  Either (String, Pos) (Maybe AstType)
toType _ _ (AstExprInt pos _) = Right $ Just $ AstTypeI32 pos
toType funcs vars (AstExprVar pos ident) =
  case lookup ident vars of
    Just type' -> Right $ Just type'
    Nothing ->
      case lookup ident funcs of
        Just (Sig _ argTypes returnType) ->
          Right $ Just $ AstTypeFunc pos argTypes returnType
        Nothing -> Left $ typeError pos
toType funcs vars (AstExprUnOp pos _ expr) =
  case toType funcs vars expr of
    Right (Just (AstTypeI32 _)) -> Right $ Just $ AstTypeI32 pos
    Right _ -> Left $ typeError pos
    l@(Left _) -> l
toType funcs vars (AstExprBinOp pos left _ right) =
  case toType funcs vars left of
    Right (Just (AstTypeI32 _)) ->
      case toType funcs vars right of
        Right (Just (AstTypeI32 _)) -> Right $ Just $ AstTypeI32 pos
        Right _ -> Left $ typeError pos
        l@(Left _) -> l
    Right _ -> Left $ typeError pos
    l@(Left _) -> l
toType funcs vars (AstExprCall pos expr args) =
  case findCallType funcs vars expr of
    Just (AstTypeFunc _ argTypes returnType) ->
      case mapM (toType funcs vars) args of
        Right types ->
          if and $ zipWith (==) argTypes $ catMaybes types
            then Right returnType
            else Left $ typeError pos
        Left e -> Left e
    _ -> Left $ typeError pos

checkStmt ::
  Map String Sig ->
  Map String AstType ->
  AstStmt ->
  Either (String, Pos) (Map String AstType)
checkStmt _ _ _ = undefined

checkFunc :: Map String Sig -> AstFunc -> Either (String, Pos) AstFunc
checkFunc sigs func =
  func <$ foldM (checkStmt sigs) empty (getAstFuncAst func)

checkFuncs :: [AstFunc] -> Either (String, Pos) [AstFunc]
checkFuncs funcs = mapM (checkFunc sigs) funcs
  where
    sigs = fromList $ map toSig funcs
