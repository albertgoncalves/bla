module Main where

import Ast (Pos)
import Parse (Source, parse)
import PreCompile (preCompile)
import System.Environment (getArgs, getExecutablePath)
import Text.Printf (printf)

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
    _ -> getExecutablePath >>= putStrLn . printf "$ %s path/to/script.bla"
