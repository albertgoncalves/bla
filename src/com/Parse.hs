module Parse where

import Ast
  ( AstExpr (..),
    AstPreFunc (..),
    AstStmt (..),
    AstType (..),
    BinOp (..),
    Pos,
    UnOp (..),
  )
import Control.Applicative ((<|>))
import Control.Monad ((<=<))
import Data.Char (isAlphaNum, isDigit, isLower, isSpace)
import Data.List (stripPrefix)

type Source = String

data Token
  = TokenEnd
  | TokenLParen Pos
  | TokenRParen Pos
  | TokenLBracket Pos
  | TokenRBracket Pos
  | TokenLBrace Pos
  | TokenRBrace Pos
  | TokenArrow Pos
  | TokenComma Pos
  | TokenSemiC Pos
  | TokenDoubleC Pos
  | TokenEq Pos
  | TokenAssign Pos
  | TokenLT Pos
  | TokenGT Pos
  | TokenBang Pos
  | TokenAdd Pos
  | TokenSub Pos
  | TokenMul Pos
  | TokenDiv Pos
  | TokenInt Pos Int
  | TokenIdent Pos String
  | TokenIntrin Pos String
  | TokenKeyword Pos String
  deriving (Eq, Show)

getPos :: Token -> Int
getPos TokenEnd = 0
getPos (TokenLParen p) = p
getPos (TokenRParen p) = p
getPos (TokenLBracket p) = p
getPos (TokenRBracket p) = p
getPos (TokenLBrace p) = p
getPos (TokenRBrace p) = p
getPos (TokenArrow p) = p
getPos (TokenComma p) = p
getPos (TokenSemiC p) = p
getPos (TokenDoubleC p) = p
getPos (TokenEq p) = p
getPos (TokenAssign p) = p
getPos (TokenLT p) = p
getPos (TokenGT p) = p
getPos (TokenBang p) = p
getPos (TokenAdd p) = p
getPos (TokenSub p) = p
getPos (TokenMul p) = p
getPos (TokenDiv p) = p
getPos (TokenInt p _) = p
getPos (TokenIdent p _) = p
getPos (TokenIntrin p _) = p
getPos (TokenKeyword p _) = p

isIdent :: Char -> Bool
isIdent c = isAlphaNum c || c == '_'

keyword :: Source -> String -> Maybe (Token, Source)
keyword cs0 s =
  case stripPrefix s cs0 of
    Nothing -> Nothing
    Just cs1 -> Just (TokenKeyword (length cs0) s, cs1)

keywords :: Source -> Maybe (Token, Source)
keywords cs =
  foldr1 (<|>) $
    map
      (keyword cs)
      [ "if",
        "as",
        "loop",
        "break",
        "continue",
        "return",
        "addr",
        "fn",
        "i32"
      ]

dropThru :: Eq a => (a -> Bool) -> [a] -> [a]
dropThru _ [] = []
dropThru f (x : xs)
  | f x = xs
  | otherwise = dropThru f xs

token :: Source -> Either Pos (Token, Source)
token [] = Right (TokenEnd, [])
token ('#' : cs) = maybeKeyword $ dropThru (== '\n') cs
token cs'@('_' : cs) = Right (TokenIdent (length cs') "_", cs)
token cs'@('(' : cs) = Right (TokenLParen $ length cs', cs)
token cs'@(')' : cs) = Right (TokenRParen $ length cs', cs)
token cs'@('[' : cs) = Right (TokenLBracket $ length cs', cs)
token cs'@(']' : cs) = Right (TokenRBracket $ length cs', cs)
token cs'@('{' : cs) = Right (TokenLBrace $ length cs', cs)
token cs'@('}' : cs) = Right (TokenRBrace $ length cs', cs)
token cs'@(',' : cs) = Right (TokenComma $ length cs', cs)
token cs'@(';' : cs) = Right (TokenSemiC $ length cs', cs)
token cs'@('-' : '>' : cs) = Right (TokenArrow $ length cs', cs)
token cs'@(':' : ':' : cs) = Right (TokenDoubleC $ length cs', cs)
token cs'@('=' : '=' : cs) = Right (TokenEq $ length cs', cs)
token cs'@('=' : cs) = Right (TokenAssign $ length cs', cs)
token cs'@('<' : cs) = Right (TokenLT $ length cs', cs)
token cs'@('>' : cs) = Right (TokenGT $ length cs', cs)
token cs'@('!' : cs) = Right (TokenBang $ length cs', cs)
token cs'@('+' : cs) = Right (TokenAdd $ length cs', cs)
token cs'@('-' : cs) = Right (TokenSub $ length cs', cs)
token cs'@('*' : cs) = Right (TokenMul $ length cs', cs)
token cs'@('/' : cs) = Right (TokenDiv $ length cs', cs)
token cs'@('@' : cs) =
  let (as, bs) = span isIdent cs
   in Right (TokenIntrin (length cs') ('@' : as), bs)
token cs'@(c : cs)
  | isDigit c =
    let (as, bs) = span isDigit cs in Right (TokenInt p (read $ c : as), bs)
  | isLower c =
    let (as, bs) = span isIdent cs in Right (TokenIdent p (c : as), bs)
  | isSpace c = maybeKeyword $ dropWhile isSpace cs
  | otherwise = Left p
  where
    p = length cs'

maybeKeyword :: Source -> Either Pos (Token, Source)
maybeKeyword as0 =
  case keywords as0 of
    Just (b, as1) -> return (b, as1)
    Nothing -> token as0

tokens :: Source -> Either Pos [Token]
tokens [] = Right []
tokens as0 = do
  (b, as1) <- maybeKeyword as0
  bs <- tokens as1
  return $ b : bs

typedArg :: [Token] -> Either Pos ((String, AstType), [Token])
typedArg (TokenIdent _ ident : ts0) = do
  (argType, ts1) <- type' ts0
  return ((ident, argType), ts1)
typedArg (t : _) = Left $ getPos t
typedArg [] = Left 0

commaDelim ::
  ([Token] -> Either Pos (a, [Token])) ->
  [Token] ->
  Either Pos ([a], [Token])
commaDelim f ts0 = do
  (x, ts1) <- f ts0
  case ts1 of
    (TokenComma _ : ts2) -> do
      (xs, ts3) <- commaDelim f ts2
      return (x : xs, ts3)
    _ -> return ([x], ts1)

type' :: [Token] -> Either Pos (AstType, [Token])
type' (TokenKeyword p "i32" : ts) = Right (AstTypeI32 p, ts)
type' (TokenKeyword p "addr" : ts) = Right (AstTypeAddr p, ts)
type' (TokenKeyword p "fn" : TokenLParen _ : TokenRParen _ : ts0) =
  case ts0 of
    (TokenArrow _ : ts1) -> do
      (returnType, ts2) <- type' ts1
      return (AstTypeFunc p [] (Just returnType), ts2)
    ts1 -> Right (AstTypeFunc p [] Nothing, ts1)
type' (TokenKeyword p "fn" : TokenLParen _ : ts0) = do
  (argTypes, ts1) <- commaDelim type' ts0
  case ts1 of
    (TokenRParen _ : TokenArrow _ : ts2) -> do
      (returnType, ts3) <- type' ts2
      return (AstTypeFunc p argTypes (Just returnType), ts3)
    (TokenRParen _ : ts2) ->
      return (AstTypeFunc p argTypes Nothing, ts2)
    (t : _) -> Left $ getPos t
    [] -> Left 0
type' (t : _) = Left $ getPos t
type' [] = Left 0

precPrefix :: Token -> Either Pos (Int, UnOp)
precPrefix (TokenBang _) = Right (9, UnOpBang)
precPrefix (TokenSub _) = Right (9, UnOpMinus)
precPrefix t = Left $ getPos t

precInfix :: Token -> Either Pos (Int, Int, BinOp)
precInfix (TokenAdd _) = Right (5, 6, BinOpAdd)
precInfix (TokenSub _) = Right (5, 6, BinOpSub)
precInfix (TokenMul _) = Right (7, 8, BinOpMul)
precInfix (TokenDiv _) = Right (7, 8, BinOpDiv)
precInfix (TokenEq _) = Right (3, 4, BinOpEq)
precInfix t = Left $ getPos t

precParen :: Int
precParen = 11

precBracket :: Int
precBracket = 11

precAs :: Int
precAs = 11

exprLeft :: [Token] -> Either Pos (AstExpr, [Token])
exprLeft [] = Left 0
exprLeft (TokenLParen _ : ts0) = do
  r <- expr ts0 0
  case r of
    (value, TokenRParen _ : ts1) -> Right (value, ts1)
    (_, t : _) -> Left $ getPos t
    _ -> Left 0
exprLeft (TokenIdent p ident : ts) = Right (AstExprVar p ident, ts)
exprLeft (TokenIntrin p intrinsic : ts) = Right (AstExprVar p intrinsic, ts)
exprLeft (TokenInt p n : ts) = Right (AstExprInt p n, ts)
exprLeft (t : ts0) = do
  (prec, op) <- precPrefix t
  (value, ts1) <- expr ts0 prec
  return (AstExprUnOp (getPos t) op value, ts1)

exprArgs :: [Token] -> Either Pos ([AstExpr], [Token])
exprArgs [] = Left 0
exprArgs ts0 = do
  r <- expr ts0 0
  case r of
    (arg, TokenComma _ : ts1) -> do
      (args, ts2) <- exprArgs ts1
      return (arg : args, ts2)
    (arg, ts1) -> Right ([arg], ts1)

exprRight :: AstExpr -> [Token] -> Int -> Either Pos (AstExpr, [Token])
exprRight value [] _ = Right (value, [])
exprRight value (TokenLParen p : TokenRParen _ : ts) prec =
  exprRight (AstExprCall p value []) ts prec
exprRight value ts0'@(TokenLParen p : ts0) prec =
  if precParen < prec
    then Right (value, ts0')
    else do
      r <- exprArgs ts0
      case r of
        (args, TokenRParen _ : ts1) ->
          exprRight (AstExprCall p value args) ts1 prec
        (_, t : _) -> Left $ getPos t
        (_, []) -> Left 0
exprRight value ts0'@(TokenLBracket p : ts0) prec =
  if precBracket < prec
    then Right (value, ts0')
    else do
      r <- expr ts0 0
      case r of
        (arg, TokenRBracket _ : ts1) ->
          exprRight (AstExprRead p value arg) ts1 prec
        (_, t : _) -> Left $ getPos t
        (_, []) -> Left 0
exprRight value ts0'@(TokenKeyword p "as" : ts0) prec =
  if precAs < prec
    then Right (value, ts0')
    else do
      (valueType, ts1) <- type' ts0
      exprRight (AstExprAs p value valueType) ts1 prec
exprRight left ts0'@(t : ts0) prec =
  case precInfix t of
    Right (precInfixL, precInfixR, op) ->
      if precInfixL < prec
        then Right (left, ts0')
        else do
          (right, ts1) <- expr ts0 precInfixR
          exprRight (AstExprBinOp (getPos t) left op right) ts1 prec
    Left _ -> Right (left, ts0')

-- NOTE: See `https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html`.
expr :: [Token] -> Int -> Either Pos (AstExpr, [Token])
expr ts0 prec0 = do
  r0 <- exprLeft ts0
  case r0 of
    (e, []) -> Right (e, [])
    (left, ts1) -> exprRight left ts1 prec0

statement :: [Token] -> Either Pos (AstStmt, [Token])
statement [] = Left 0
statement (TokenKeyword p "if" : ts0) = do
  (condition, ts1) <- expr ts0 0
  case ts1 of
    (TokenLBrace _ : ts2) -> do
      (body, ts3) <- statements ts2
      case ts3 of
        (TokenRBrace _ : ts4) -> return (AstStmtIf p condition body, ts4)
        (t : _) -> Left $ getPos t
        [] -> Left 0
    (t : _) -> Left $ getPos t
    [] -> Left 0
statement (TokenKeyword p "loop" : TokenLBrace _ : ts0) = do
  (body, ts1) <- statements ts0
  case ts1 of
    (TokenRBrace _ : ts2) -> return (AstStmtLoop p body, ts2)
    (t : _) -> Left $ getPos t
    [] -> Left 0
statement (TokenIdent p "_" : TokenAssign _ : ts0) = do
  (value, ts1) <- expr ts0 0
  case ts1 of
    (TokenSemiC _ : ts2) -> return (AstStmtDiscard p value, ts2)
    (t : _) -> Left $ getPos t
    [] -> Left 0
statement (TokenIdent p ident : TokenAssign _ : ts0) = do
  (value, ts1) <- expr ts0 0
  case ts1 of
    (TokenSemiC _ : ts2) -> return (AstStmtAssign p ident value, ts2)
    (t : _) -> Left $ getPos t
    [] -> Left 0
statement (TokenKeyword p "break" : TokenInt _ n : TokenSemiC _ : ts) =
  Right (AstStmtBreak p n, ts)
statement (TokenKeyword _ "break" : t : _) = Left $ getPos t
statement (TokenKeyword p "continue" : TokenInt _ n : TokenSemiC _ : ts) =
  Right (AstStmtCont p n, ts)
statement (TokenKeyword _ "continue" : t : _) = Left $ getPos t
statement (TokenKeyword p "return" : TokenSemiC _ : ts) =
  Right (AstStmtRet p Nothing, ts)
statement (TokenKeyword p "return" : ts0) = do
  (value, ts1) <- expr ts0 0
  case ts1 of
    (TokenSemiC _ : ts2) -> return (AstStmtRet p (Just value), ts2)
    (t : _) -> Left $ getPos t
    [] -> Left 0
statement ts0@(t0 : _) = do
  (value1, ts1) <- expr ts0 0
  case (value1, ts1) of
    (AstExprRead p base offset, TokenAssign _ : ts2) -> do
      (value3, ts3) <- expr ts2 0
      case ts3 of
        (TokenSemiC _ : ts4) -> Right (AstStmtSave p base offset value3, ts4)
        (t1 : _) -> Left $ getPos t1
        [] -> Left 0
    (_, TokenSemiC _ : ts2) -> Right (AstStmtEffect (getPos t0) value1, ts2)
    (_, t1 : _) -> Left $ getPos t1
    (_, []) -> Left 0

statements :: [Token] -> Either Pos ([AstStmt], [Token])
statements ts0 = do
  (x, ts1) <- statement ts0
  case ts1 of
    (TokenRBrace _ : _) -> return ([x], ts1)
    ts2 -> do
      (xs, ts3) <- statements ts2
      return (x : xs, ts3)

func :: [Token] -> Either Pos (AstPreFunc, [Token])
func [] = Left 0
func (TokenIdent p ident : ts0) =
  case ts0 of
    (TokenLParen _ : TokenRParen _ : TokenLBrace _ : ts1) -> do
      (body, ts2) <- statements ts1
      case ts2 of
        (TokenRBrace _ : ts3) ->
          Right (AstPreFunc p ident [] Nothing body, ts3)
        (t : _) -> Left $ getPos t
        [] -> Left 0
    (TokenLParen _ : TokenRParen _ : TokenDoubleC _ : ts1) -> do
      (returnType, ts2) <- type' ts1
      case ts2 of
        (TokenLBrace _ : ts3) -> do
          (body, ts4) <- statements ts3
          case ts4 of
            (TokenRBrace _ : ts5) ->
              Right (AstPreFunc p ident [] (Just returnType) body, ts5)
            (t : _) -> Left $ getPos t
            [] -> Left 0
        (t : _) -> Left $ getPos t
        [] -> Left 0
    (TokenLParen _ : ts1) -> do
      (argTypes, ts2) <- commaDelim typedArg ts1
      case ts2 of
        (TokenRParen _ : TokenDoubleC _ : ts3) -> do
          (returnType, ts4) <- type' ts3
          case ts4 of
            (TokenLBrace _ : ts5) -> do
              (body, ts6) <- statements ts5
              case ts6 of
                (TokenRBrace _ : ts7) ->
                  Right
                    (AstPreFunc p ident argTypes (Just returnType) body, ts7)
                (t : _) -> Left $ getPos t
                [] -> Left 0
            (t : _) -> Left $ getPos t
            [] -> Left 0
        (TokenRParen _ : ts3) -> do
          case ts3 of
            (TokenLBrace _ : ts4) -> do
              (body, ts5) <- statements ts4
              case ts5 of
                (TokenRBrace _ : ts6) ->
                  Right (AstPreFunc p ident argTypes Nothing body, ts6)
                (t : _) -> Left $ getPos t
                [] -> Left 0
            (t : _) -> Left $ getPos t
            [] -> Left 0
        (t : _) -> Left $ getPos t
        [] -> Left 0
    (t : _) -> Left $ getPos t
    [] -> Left 0
func (t : _) = Left $ getPos t

program :: [Token] -> Either Pos [AstPreFunc]
program ts0 = do
  r <- func ts0
  case r of
    (f, [TokenEnd]) -> Right [f]
    (f, ts1) -> do
      fs <- program ts1
      return $ f : fs

parse :: Source -> Either (String, Pos) [AstPreFunc]
parse = either (Left . (,) "parse") Right . (program <=< tokens)
