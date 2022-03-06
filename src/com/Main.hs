import Ast (Pos)
import Compile (assemble, compile)
import Data.Tuple (swap)
import Emit (emit)
import Parse (Source, parse)
import PreCompile (preCompile)
import System.Environment (getArgs, getExecutablePath)
import System.Exit (die)
import System.IO (hPutStrLn, stderr)
import Text.Printf (printf)
import Type (checkFuncs)

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

main :: IO ()
main = do
  args <- getArgs
  case args of
    [pathIn, pathOut] -> do
      source <- readFile pathIn
      either
        (die . uncurry (showError pathIn source) . swap)
        (emit pathOut . assemble . compile)
        (parse source >>= checkFuncs >>= preCompile)
    _ ->
      getExecutablePath
        >>= hPutStrLn stderr
          . printf "$ %s path/to/script.bla path/to/bytecode.blc"
