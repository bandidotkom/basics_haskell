-- Original version: Simon Thompson

module CodeTable ( codeTable )
     where

import HTypes

codeTable :: Tree -> HTable
codeTable = convert []

convert :: HCode -> Tree -> HTable
convert hc (Leaf c n) = [(c,hc)]
convert hc (Node n tl tr) = (convert (hc++[L]) tl) ++ (convert (hc++[R]) tr)

-- Original version: Simon Thompson

module Coding ( code, decode )
      where

import HTypes ( Tree ( Leaf, Node ), Bit ( L, R ), HCode, HTable )

code :: HTable -> [Char] -> HCode
code tbl = concat . map (lookup' tbl)

lookup' :: HTable -> Char -> HCode
lookup' [] c = error "lookup'..."
lookup' ((ch,n):rtb) c | ch==c  = n
                      | otherwise = lookup' rtb c

decode :: Tree -> HCode -> [Char]
decode htree = decodeByte htree
               where
                 decodeByte (Node n t1 t2) (L:rest) = decodeByte t1 rest
                 decodeByte (Node n t1 t2) (R:rest) = decodeByte t2 rest
                 decodeByte (Leaf c n) rest = c:decodeByte htree rest
                 decodeByte t [] = []

-- Original version: Simon Thompson

module Frequency (frequency) -- [Char] -> [(Char,Int)]
      where

-- general merge sort whisch uses two sort functions

mergeSort :: ([a]->[a]->[a])-> [a] -> [a]
mergeSort merge xs | (length xs < 2) = xs
                   | otherwise     = merge (mergeSort merge first)
                                           (mergeSort merge second)
                                     where
                                        first = take half xs
                                        second = drop half xs
                                        half = (length xs) `div`2

-- in sorting the characters, we amalgamate entries for the same charaters

alphaMerge :: [(Char, Int)] -> [(Char,Int)] -> [(Char,Int)]
alphaMerge xs [] = xs
alphaMerge [] ys = ys
alphaMerge ((p,n):xs) ((q,m):ys)
           | (p==q)   = (p,n+m) : alphaMerge xs ys
           | (p<q)    = (p,n) : alphaMerge xs ((q,m):ys)
           | otherwise = (q,m) : alphaMerge ((p,n):xs) ys

-- when two pairs have the same frequency
-- we order according to the character ordering

freqMerge :: [(Char, Int)] -> [(Char,Int)] -> [(Char,Int)]
freqMerge xs [] = xs
freqMerge [] ys = ys
freqMerge ((p,n):xs) ((q,m):ys)
           | (n<m || (n==m && p<q)) = (p,n) : freqMerge xs ((q,m):ys)
           | otherwise = (q,m) : freqMerge ((p,n):xs) ys

frequency :: [Char] -> [(Char,Int)]
frequency = mergeSort freqMerge . mergeSort alphaMerge . map start
            where
             start ch = (ch,1)

-- Original version: Simon Thompson

module HTypes (
                Tree ( Leaf, Node ),
                Bit ( L, R ),
                HCode,
                HTable
              )
        where

data Bit = L | R 
               deriving (Eq, Show)

type HCode = [Bit]

-- in the translation we convert the Huffman tree to a table 

type HTable = [ (Char, HCode) ]

-- The Huffman trees carry characters at the leaves
-- and frequencies at the internal nodes.

data Tree = Leaf Char Int | Node Int Tree Tree
          deriving (Show)

-- module for testing

module Main (main) where

import HTypes ( Tree(Leaf,Node), Bit(L,R), HCode, HTable )
import Coding ( code, decode )
import MakeCode ( codes, codeTable )
import Frequency ( frequency )
import MakeTree( makeTree, toTreeList )


text = "WELCHE BUCHSTABEN WERDEN IN DER DEUTSCHEN SPRACHE AM HAEUFIGSTEN VERWENDET?"
text_freq = frequency text

htree = makeTree text_freq
htable = codeTable htree

htree_example = Node 10 (Node 20 (Leaf 'a' 3) (Node 30 (Leaf 'b' 6) (Leaf 'c' 3)))
                     (Node 10 (Leaf 'd' 2) (Leaf 'e' 5))

hcode = code htable "BBADDR VWE"
test_decode = decode htree hcode

list_of_trees = toTreeList text_freq 
test_codes = codes text

-- Original version: Simon Thompson

module MakeCode ( codes, codeTable )
       where

import HTypes  ( Tree ( Leaf, Node ), Bit ( L, R ), HCode, HTable )
import Frequency ( frequency )
import MakeTree ( makeTree )
import CodeTable ( codeTable )

-- frequency calculation and tree Conversion

codes :: [Char] -> Tree
codes = makeTree . frequency

-- Original version: Simon Thompson

module MakeTree ( makeTree, toTreeList )  -- [(Char,Int)] -> Tree
      where

import HTypes ( Tree ( Leaf, Node ), Bit ( L, R ), HCode, HTable )

makeTree :: [(Char,Int)] -> Tree
makeTree = makeCodes . toTreeList

toTreeList ::  [(Char,Int)] -> [Tree]
toTreeList = map (uncurry Leaf)

-- uncurry converts a curried function to a function on pairs
-- (a -> b -> c) -> (a,b) -> c

-- curry converts an uncurried function to a curried function
-- ((a,b) -> c) -> a -> b -> c

makeCodes :: [Tree] -> Tree
makeCodes [t] = t
makeCodes ts = makeCodes (amalgamate ts)

amalgamate :: [Tree] -> [Tree]
amalgamate (t1:t2:ts) = insertTree (pair t1 t2) ts

pair :: Tree -> Tree -> Tree
pair t1 t2 = Node (f1+f2) t1 t2
             where
             f1 = value t1
             f2 = value t2

value :: Tree -> Int
value (Leaf _ n) = n
value (Node n _ _) = n

insertTree ::Tree -> [Tree] -> [Tree]
insertTree t [] = [t]
insertTree t (t1:ts) | value t < value t1 = t:t1:ts
                     | otherwise = t1 : insertTree t ts


