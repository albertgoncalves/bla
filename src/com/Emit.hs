module Emit where

import Compile (Inst (..))
import Data.ByteString.Builder (toLazyByteString, word32LE)
import Data.ByteString.Lazy (writeFile)
import Prelude hiding (writeFile)

toInt :: Inst -> [Int]
toInt InstHalt = [0]
toInt (InstPush x) = [1, x]
toInt (InstCopy x) = [2, x]
toInt (InstStore x) = [3, x]
toInt (InstDrop x) = [4, x]
toInt (InstRsrv x) = [5, x]
toInt InstSwap = [6]
toInt InstJump = [7]
toInt InstJifz = [8]
toInt InstAdd = [9]
toInt InstSub = [10]
toInt InstMul = [11]
toInt InstDiv = [12]
toInt InstEq = [13]
toInt InstNeg = [14]
toInt InstNot = [15]
toInt InstPrCh = [100]
toInt InstPrI32 = [101]
toInt (PreInstLabelSet _) = undefined
toInt (PreInstLabelPush _) = undefined

emit :: FilePath -> [Inst] -> IO ()
emit path =
  writeFile path
    . toLazyByteString
    . mconcat
    . map (word32LE . fromIntegral)
    . concatMap toInt
