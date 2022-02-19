module Emit where

import Compile (Inst (..))
import Data.ByteString.Builder (toLazyByteString, word32LE)
import Data.ByteString.Lazy (writeFile)
import Prelude hiding (writeFile)

toInt :: Inst -> Int
toInt InstHalt = 0
toInt InstPush = 1
toInt InstCopy = 2
toInt InstStore = 3
toInt InstDrop = 4
toInt InstRsrv = 5
toInt InstSwap = 6
toInt InstJump = 7
toInt InstJifz = 8
toInt InstAdd = 9
toInt InstSub = 10
toInt InstMul = 11
toInt InstDiv = 12
toInt InstEq = 13
toInt InstNeg = 14
toInt InstNot = 15
toInt (InstLitInt x) = x
toInt (PreInstLabelSet _) = undefined
toInt (PreInstLabelPush _) = undefined

emit :: FilePath -> [Inst] -> IO ()
emit path =
  writeFile path
    . toLazyByteString
    . mconcat
    . map (word32LE . fromIntegral . toInt)
