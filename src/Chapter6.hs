{-# LANGUAGE OverloadedStrings, RankNTypes #-}

-- Folds
module Chapter6 where

import           Control.Lens
import           Data.Char
import qualified Data.Map                      as M
import qualified Data.Set                      as S
import qualified Data.Text                     as T

beastSizes :: [(Int, String)]
beastSizes = [(3, "Sirens"), (882, "Kraken"), (92, "Ogopogo")]

folds_6_1_1 :: [(Int, String)]
folds_6_1_1 = beastSizes ^.. folded

folds_6_1_2 :: [String]
folds_6_1_2 = beastSizes ^.. folded . folded

folds_6_1_3 :: String
folds_6_1_3 = beastSizes ^.. folded . folded . folded

folds_6_1_4 :: [String]
folds_6_1_4 = beastSizes ^.. folded . _2

folds_6_1_5 :: [Int]
folds_6_1_5 = toListOf (folded . folded) [[1, 2, 3], [4, 5, 6]]

-- if the data is written in-line, it does not compile due to OverloadedStrings (needed for `quotes`, not mentioned in the book)
data_6_1_6 :: M.Map String String
data_6_1_6 = M.fromList [("Jack", "Captain"), ("Will", "First Mate")]

folds_6_1_6 :: String
folds_6_1_6 = toListOf (folded . folded) data_6_1_6

-- same issue with the data in-line
data_6_1_7 :: (String, String)
data_6_1_7 = ("Hello", "It's me")

folds_6_1_7 :: String
folds_6_1_7 = data_6_1_7 ^.. both . folded

folds_6_1_8 :: [String]
folds_6_1_8 = ("Why", "So", "Serious?") ^.. each

quotes :: [(T.Text, T.Text, T.Text)]
quotes = [("Why", "So", "Serious?"), ("This", "is", "SPARTA")]

folds_6_1_9 :: String
folds_6_1_9 = quotes ^.. each . each . each

folds_6_2_1 = h (f . g) [(1, 'a'), (2, 'b'), (3, 'c')]
 where
  f :: Fold [(Int, Char)] (Int, Char)
  f = folded
  g :: Fold (Int, Char) Int
  g = _1
  h :: Fold [(Int, Char)] Int -> [(Int, Char)] -> [Int]
  h = toListOf
