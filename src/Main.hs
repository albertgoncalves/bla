{-# LANGUAGE LambdaCase #-}

import Control.Applicative (Alternative (..))
import Data.Bifunctor (first)
import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)
import Data.List (nub)
import Data.Maybe (fromMaybe)
import System.Environment (getArgs, getExecutablePath)
import Text.Printf (printf)

data UnOp
  = UnOpBang
  | UnOpMinus
  deriving (Show)

data BinOp
  = BinOpAdd
  | BinOpSub
  | BinOpMul
  | BinOpDiv
  | BinOpEq
  deriving (Show)

type Pos = Int

data AstExpr
  = AstExprInt Pos Int
  | AstExprVar Pos String
  | AstExprUnOp Pos UnOp AstExpr
  | AstExprBinOp Pos AstExpr BinOp AstExpr
  | AstExprCall Pos String [AstExpr]
  deriving (Show)

data AstStmt
  = AstStmtAssign Pos String AstExpr
  | AstStmtIf Pos AstExpr [AstStmt]
  | AstStmtLoop Pos [AstStmt]
  | AstStmtBreak Pos Int
  | AstStmtCont Pos Int
  | AstStmtRet Pos AstExpr
  deriving (Show)

data AstPreFunc = AstPreFunc
  { getAstPreFuncPos :: Pos,
    getAstPreFuncName :: String,
    getAstPreFuncArgs :: [String],
    getAstPreFuncAst :: [AstStmt]
  }
  deriving (Show)

data AstFunc = AstFunc
  { getAstFuncPos :: Pos,
    getAstFuncName :: String,
    getAstFuncArgs :: [String],
    getAstFuncLocals :: [String],
    getAstFuncAst :: [AstStmt],
    getAstFuncRet :: AstExpr
  }
  deriving (Show)

type Source = String

-- NOTE: See `https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/parsec-paper-letter.pdf`.
-- NOTE: See `https://serokell.io/blog/parser-combinators-in-haskell`.
newtype Parser a = Parser
  { runParser :: Source -> Either (Bool, Maybe Pos) (Bool, (Source, a))
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

sepBy1 :: Parser b -> Parser c -> Parser [b]
sepBy1 p0 p1 = (:) <$> p0 <*> many (p1 *> p0)

sepBy :: Parser b -> Parser c -> Parser [b]
sepBy p0 p1 = sepBy1 p0 p1 <|> pure []

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

list :: Parser a -> Parser [a]
list p = sepBy p comma

digits :: Parser String
digits = token $ some $ satisfy isDigit

integer :: Parser Int
integer = read <$> digits <|> ((read .) . (:) <$> char '-' <*> digits)

ident :: Parser String
ident =
  token $ (:) <$> satisfy isAlpha <*> many (satisfy isAlphaNum <|> char '_')

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

call :: Parser AstExpr
call = AstExprCall <$> position <*> ident <*> parens (list expr)

expr :: Parser AstExpr
expr =
  foldr1
    (<|>)
    [ unOp,
      binOp,
      call,
      AstExprInt <$> position <*> integer,
      AstExprVar <$> position <*> ident,
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
        <$> position <*> (token (string "return") *> expr <* semicolon),
      AstStmtAssign
        <$> position <*> (ident <* token (char '=')) <*> (expr <* semicolon)
    ]

statements :: Parser [AstStmt]
statements = some statement

func :: Parser AstPreFunc
func =
  AstPreFunc
    <$> position
    <*> ident
    <*> parens (sepBy ident comma)
    <*> braces statements

program :: Parser [AstPreFunc]
program = space *> some func <* end

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
      Right $ AstFunc pos name args locals body' returnExpr
    Just (_, x) -> Left $ getStmtPos x
  where
    locals = nub $ collectAssigns body

parse :: Source -> Either (String, Pos) [AstPreFunc]
parse source =
  either
    (Left . (,) "parse" . snd)
    (Right . snd . snd)
    $ first (fmap (fromMaybe $ length source)) $ runParser program source

preCompile :: [AstPreFunc] -> Either (String, Pos) [AstFunc]
preCompile = either (Left . (,) "pre-compile") Right . mapM intoFunc

findLine :: Source -> Pos -> Int
findLine source pos =
  1 + length (filter id $ take (length source - pos) $ map (== '\n') source)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [path] -> do
      source <- readFile path
      putStr $
        either
          ( \(error', pos) ->
              printf "%s:%d | %s error\n" path (findLine source pos) error'
          )
          (unlines . map show)
          (parse source >>= preCompile)
    _ -> getExecutablePath >>= putStrLn . printf "%s path/to/script.bla"
