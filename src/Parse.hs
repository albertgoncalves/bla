{-# LANGUAGE LambdaCase #-}

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
import Control.Applicative (Alternative (..), optional)
import Data.Bifunctor (first)
import Data.Char (isAlphaNum, isDigit, isLower, isSpace)
import Data.Maybe (fromMaybe)

type Source = String

type Consumed = Bool

-- NOTE: See `https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/parsec-paper-letter.pdf`.
-- NOTE: See `https://serokell.io/blog/parser-combinators-in-haskell`.
newtype Parser a = Parser
  { runParser :: Source -> Either (Consumed, Maybe Pos) (Consumed, (Source, a))
  }

instance Functor Parser where
  fmap f (Parser p) = Parser $ fmap (fmap (fmap f)) . p

instance Applicative Parser where
  pure x = Parser $ \input -> Right (False, (input, x))
  (Parser p0) <*> (Parser p1) = Parser $ \input0 ->
    case p0 input0 of
      Right (True, (input1, f)) ->
        case p1 input1 of
          Right (_, (input2, x)) -> Right (True, (input2, f x))
          Left (_, e) -> Left (True, e)
      Right (False, (input1, f)) ->
        case p1 input1 of
          Right (True, (input2, x)) -> Right (True, (input2, f x))
          Right (False, (input2, x)) -> Right (False, (input2, f x))
          Left (True, e) -> Left (True, e)
          Left (False, e) -> Left (False, e)
      Left (True, e) -> Left (True, e)
      Left (False, e) -> Left (False, e)

instance Alternative Parser where
  empty = Parser $ const $ Left (False, Nothing)
  (Parser p0) <|> (Parser p1) = Parser $ \input ->
    case p0 input of
      r1@(Right (True, _)) -> r1
      r1@(Right (False, _)) ->
        case p1 input of
          r2@(Right (True, _)) -> r2
          l2@(Left (True, _)) -> l2
          _ -> r1
      l1@(Left (True, _)) ->
        case p1 input of
          r2@(Right (True, _)) -> r2
          _ -> l1
      Left (False, _) -> p1 input

sepBy :: Parser b -> Parser c -> Parser [b]
sepBy p0 p1 = ((:) <$> p0 <*> many (p1 *> p0)) <|> pure []

end :: Parser ()
end = Parser $ \case
  [] -> Right (False, ([], ()))
  input -> Left (False, Just $ length input)

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $ \case
  [] -> Left (False, Just 0)
  (x : xs) ->
    if f x
      then Right (True, (xs, x))
      else Left (False, Just $ length xs + 1)

position :: Parser Pos
position = Parser $ \input -> Right (False, (input, length input))

char :: Char -> Parser Char
char = satisfy . (==)

string :: String -> Parser String
string = traverse char

comment :: Parser ()
comment = () <$ (char '#' *> many (satisfy (/= '\n')) *> char '\n')

space :: Parser ()
space = () <$ many (comment <|> (() <$ satisfy isSpace))

token :: Parser a -> Parser a
token p = p <* space

parens :: Parser a -> Parser a
parens p = token (char '(') *> p <* token (char ')')

braces :: Parser a -> Parser a
braces p = token (char '{') *> p <* token (char '}')

brackets :: Parser a -> Parser a
brackets p = token (char '[') *> p <* token (char ']')

semicolon :: Parser ()
semicolon = () <$ token (char ';')

comma :: Parser ()
comma = () <$ token (char ',')

digits :: Parser String
digits = token $ some $ satisfy isDigit

integer :: Parser Int
integer = read <$> digits <|> ((read .) . (:) <$> char '-' <*> digits)

ident :: Parser String
ident =
  token $ (:) <$> satisfy isLower <*> many (satisfy isAlphaNum <|> char '_')

unOp :: Parser AstExpr
unOp = parens (foldr1 (<|>) (map f [(UnOpBang, '!'), (UnOpMinus, '-')]))
  where
    f (op, x) = AstExprUnOp <$> position <*> (op <$ token (char x)) <*> expr

binOp :: Parser AstExpr
binOp =
  parens $
    foldr1
      (<|>)
      ( map
          f
          [ (BinOpAdd, () <$ token (char '+')),
            (BinOpSub, () <$ token (char '-')),
            (BinOpMul, () <$ token (char '*')),
            (BinOpDiv, () <$ token (char '/')),
            (BinOpEq, () <$ token (string "=="))
          ]
      )
  where
    f (op, p) = AstExprBinOp <$> position <*> expr <*> (op <$ p) <*> expr

exprIdent :: Parser AstExpr
exprIdent = AstExprVar <$> position <*> ident

call :: Parser AstExpr
call =
  AstExprCall
    <$> position
    <*> (exprIdent <|> parens expr)
    <*> parens (sepBy expr comma)

expr :: Parser AstExpr
expr =
  foldr1
    (<|>)
    [ unOp,
      binOp,
      call,
      AstExprInt <$> position <*> integer,
      exprIdent,
      parens expr
    ]

statement :: Parser AstStmt
statement =
  foldr1
    (<|>)
    [ AstStmtIf
        <$> position <*> (token (string "if") *> expr) <*> braces statements,
      AstStmtLoop
        <$> position <*> (token (string "loop") *> braces statements),
      AstStmtBreak
        <$> position <*> (token (string "break") *> integer <* semicolon),
      AstStmtCont
        <$> position <*> (token (string "continue") *> integer <* semicolon),
      AstStmtRet
        <$> position
        <*> (token (string "return") *> optional expr <* semicolon),
      AstStmtAssign
        <$> position <*> (ident <* token (char '=')) <*> (expr <* semicolon),
      AstStmtDiscard
        <$> position
        <*> (token (char '_') *> token (char '=') *> expr <* semicolon),
      AstStmtEffect <$> position <*> (parens expr <* semicolon)
    ]

statements :: Parser [AstStmt]
statements = some statement

type' :: Parser AstType
type' =
  foldr1
    (<|>)
    [ AstTypeI32 <$> position <* token (string "i32"),
      AstTypeFunc
        <$> (position <* token (string "::"))
        <*> parens (sepBy type' comma)
        <*> optional type'
    ]

identType :: Parser (String, AstType)
identType = (,) <$> ident <*> type'

func :: Parser AstPreFunc
func =
  AstPreFunc
    <$> position
    <*> ident
    <*> parens (sepBy identType comma)
    <*> optional type'
    <*> braces statements

program :: Parser [AstPreFunc]
program = space *> some func <* end

parse :: Source -> Either (String, Pos) [AstPreFunc]
parse source =
  either
    (Left . (,) "parse" . snd)
    (Right . snd . snd)
    $ first (fmap (fromMaybe $ length source)) $ runParser program source
