module Main where

import Ast (Pos)
import Compile (assemble, compile)
import Data.Tuple (swap)
import Parse (Source, parse)
import PreCompile (preCompile)
import System.Environment (getArgs, getExecutablePath)
import Text.Printf (printf)
import Type (checkFuncs)
import Vm (Node (..), run)

getRowCol :: Source -> Pos -> (Int, Int)
getRowCol source pos =
  ( 1 + length (filter (== '\n') xs),
    1 + length (takeWhile (/= '\n') $ reverse xs)
  )
  where
    xs = take (length source - pos) source

showError :: FilePath -> Source -> Pos -> String -> String
showError path =
  (uncurry (printf "  %s:%d:%d [ %s error ]" path) .) . getRowCol

validate :: [a] -> [a]
validate xs@[_] = xs
validate _ = undefined

main :: IO ()
main = do
  args <- getArgs
  case args of
    [path] -> do
      source <- readFile path
      putStrLn $
        either
          (uncurry (showError path source) . swap)
          (show . getNodeInt . head . validate . run . assemble . compile)
          (parse source >>= checkFuncs >>= preCompile)
    _ -> getExecutablePath >>= putStrLn . printf "$ %s path/to/script.bla"
