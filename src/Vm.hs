{-# LANGUAGE Strict #-}

module Vm where

import Data.Array (Array, listArray, (!))

data Inst
  = InstHalt
  | InstPush
  | InstCopy
  | InstStore
  | InstDrop
  | InstRsrv
  | InstSwap
  | InstJump
  | InstJifz
  | InstAdd
  | InstSub
  | InstMul
  | InstDiv
  | InstEq
  | InstNeg
  | InstNot
  | InstLitInt Int
  | PreInstLabelSet String
  | PreInstLabelPush String
  deriving (Show)

newtype Node = Node {getNodeInt :: Int}

instance Show Node where
  show (Node x) = show x

store :: Int -> a -> [a] -> [a]
store _ _ [] = undefined
store 0 x xs = x : tail xs
store n _ xs
  | (n < 0) || (length xs < n) = undefined
store n x1 (x0 : xs) = x0 : store (n - 1) x1 xs

reserve :: Int -> a -> [a] -> [a]
reserve n a = (replicate n a ++)

swap :: [a] -> [a]
swap (b : a : xs) = a : b : xs
swap _ = undefined

pop1 :: [a] -> (a, [a])
pop1 (x : xs) = (x, xs)
pop1 _ = undefined

pop2 :: [a] -> (a, a, [a])
pop2 (b : a : xs) = (a, b, xs)
pop2 _ = undefined

lit :: Inst -> Int
lit (InstLitInt x) = x
lit _ = undefined

eval :: Array Int Inst -> Int -> [Node] -> [Node]
eval insts i xs =
  case insts ! i of
    InstHalt -> xs
    InstPush -> eval insts (i + 2) $ Node (lit $ insts ! (i + 1)) : xs
    InstCopy -> eval insts (i + 2) $ (xs !! lit (insts ! (i + 1))) : xs
    InstStore ->
      eval insts (i + 2) $ store (lit $ insts ! (i + 1)) (head xs) $ tail xs
    InstDrop -> eval insts (i + 2) $ drop (lit $ insts ! (i + 1)) xs
    InstRsrv ->
      eval insts (i + 2) $ reserve (lit $ insts ! (i + 1)) (Node 0) xs
    InstSwap -> eval insts (i + 1) $ swap xs
    InstJump -> eval insts (getNodeInt $ head xs) $ tail xs
    InstJifz ->
      let (a, b, xs') = pop2 xs
       in if getNodeInt b == 0
            then eval insts (getNodeInt a) xs'
            else eval insts (i + 1) xs'
    InstAdd ->
      let (a, b, xs') = pop2 xs
       in eval insts (i + 1) (Node (getNodeInt a + getNodeInt b) : xs')
    InstSub ->
      let (a, b, xs') = pop2 xs
       in eval insts (i + 1) (Node (getNodeInt a - getNodeInt b) : xs')
    InstMul ->
      let (a, b, xs') = pop2 xs
       in eval insts (i + 1) (Node (getNodeInt a * getNodeInt b) : xs')
    InstDiv ->
      let (a, b, xs') = pop2 xs
       in eval insts (i + 1) (Node (getNodeInt a `div` getNodeInt b) : xs')
    InstEq ->
      let (a, b, xs') = pop2 xs
       in eval
            insts
            (i + 1)
            (Node (if getNodeInt a == getNodeInt b then 1 else 0) : xs')
    InstNeg ->
      let (x, xs') = pop1 xs
       in eval insts (i + 1) (Node (negate (getNodeInt x)) : xs')
    InstNot ->
      let (x, xs') = pop1 xs
       in eval insts (i + 1) (Node (if getNodeInt x == 0 then 1 else 0) : xs')
    InstLitInt _ -> undefined
    PreInstLabelPush _ -> undefined
    PreInstLabelSet _ -> undefined

run :: [Inst] -> [Node]
run [] = undefined
run xs = eval (listArray (0, length xs - 1) xs) 0 []
