{-# LANGUAGE OverloadedStrings, RankNTypes #-}

-- Folds
module Chapter6 where

import           Control.Lens
import           Data.Char                      ( isAlpha )
import           Data.Function                  ( on )
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
-- error: ambiguous type variables given on the use of `folded`.
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

folds_6_2_2 = h (f . g) (False, S.fromList ["one", "two", "three"])
 where
  f :: Fold (Bool, S.Set String) (S.Set String)
  f = _2
  g :: Fold (S.Set String) String
  g = folded
  h :: Fold (Bool, S.Set String) String -> (Bool, S.Set String) -> [String]
  h = toListOf

folds_6_2_3 = h (f . g)
                (M.fromList [("Jack", "Captain"), ("Will", "First Mate")])
 where
  f :: Fold (M.Map String String) String
  f = folded
  g :: Fold String Char
  g = folded
  h :: Fold (M.Map String String) Char -> M.Map String String -> String
  h = toListOf

folds_6_3_1 = [1, 2, 3] ^.. each -- [1,2,3]

folds_6_3_2 = ("Light", "Dark") ^.. _1 -- ["Light"]

folds_6_3_3 = [("Light", "Dark"), ("Happy", "Sad")] ^.. folded . each -- ["Light", "Dark", "Happy", "Sad"]

folds_6_3_4 = [("Light", "Dark"), ("Happy", "Sad")] ^.. folded . _1 -- ["Light", "Happy"]

folds_6_3_5 = [("Light", "Dark"), ("Happy", "Sad")] ^.. folded . folded . f -- "DarkSad"
 where
  f :: Fold String Char
  f = folded

folds_6_3_6 = ("Bond", "James", "Bond") ^.. each

------------------------------- custom folds ------------------------------------------------

-- ambiguous error again
folds_6_4_1 :: String
folds_6_4_1 = ["Yer", "a", "wizard", "Harry"] ^.. folded . f -- "YerawizardHarry"
 where
  f :: Fold String Char
  f = folded

folds_6_4_2 = [[1, 2, 3], [4, 5, 6]] ^.. folded . folding (take 2) -- [1, 2, 4, 5]

folds_6_4_3 = [[1, 2, 3], [4, 5, 6]] ^.. folded . to (take 2) -- [[1, 2], [4, 5]]

folds_6_4_4 = ["bob", "otto", "hannah"] ^.. folded . to reverse -- ["bob", "otto", "hannah"]

folds_6_4_5 =
  ("abc", "def") ^.. folding (\(a, b) -> [a, b]) . to reverse . folded -- "cbafed

folds_6_4_6_1 = [1 .. 5] ^.. folded . to (* 100) -- [100, 200, 300, 400, 500]

folds_6_4_6_2 = (1, 2) ^.. each -- [1, 2]

folds_6_4_6_3 = [(1, "one"), (2, "two")] ^.. folded . _2 -- ["one", "two"]

folds_6_4_6_4 = (Just 1, Just 2, Just 3) ^.. each . folded -- [1, 2, 3]

folds_6_4_6_5 = (Left 1, Right 2, Left 3) ^.. each . folded -- [2]

folds_6_4_6_6 = [([1, 2], [3, 4]), ([5, 6], [7, 8])] ^.. folded . each . folded -- [1..8]

folds_6_4_6_7 =
  [1 .. 4] ^.. folded . to (\x -> if even x then Right x else Left x) -- [Left 1, Right 2, Left 3, Right 4]

folds_6_4_6_8 =
  [(1, (2, 3)), (4, (5, 6))] ^.. each . folding (\(a, (b, c)) -> [a, b, c]) -- [1..6]

folds_6_4_6_9 = [(Just 1, Left "one"), (Nothing, Right 2)] ^.. folded . folding
  (\(a, b) -> a ^.. folded <> b ^.. folded) -- [1, 2]

folds_6_4_6_10 =
  [(1, "one"), (2, "two")] ^.. folded . folding (\(a, b) -> [Left a, Right b]) -- [Left 1, Right "one", Left 2, Right "two"]

folds_6_4_6_11 = S.fromList ["apricots", "apples"] ^.. folded . folding reverse -- "selppastocirpa"

folds_6_4_7_1 =
  [(12, 45, 66), (91, 123, 87)] ^.. folded . _2 . to show . folding reverse -- "54321"

folds_6_4_7_2 = [(1, "a"), (2, "b"), (3, "c"), (4, "d")] ^.. folded . folding
  (\(a, b) -> [ b | even a ]) -- ["b", "d"]

------------------------ 6.3 fold actions --------------------------------------

actions_1_1 = has folded [] -- False

-- could also be `foldOf both`
actions_1_2 = foldOf each ("Yo", "Adrian!") -- "YoAdrian!"

actions_1_3 = elemOf each "phone" ("E.T.", "phone", "home") -- True

actions_1_4 = minimumOf folded [5, 7, 2, 3, 13, 17, 11] -- Just 2

actions_1_5 = lastOf folded [5, 7, 2, 3, 13, 17, 11] -- Just 11

actions_1_6 = anyOf folded
                    ((> 9) . length)
                    (["Bulbasur", "Charmander", "Squirtle"] :: [String]) -- True

actions_1_7 = findOf folded even [11, 22, 3, 5, 6] -- Just 22 (first even number)

actions_2_1 = findOf folded
                     (\x -> x == reverse x)
                     ["umbrella", "olives", "racecar", "hammer"] -- Just "racecar" (fist palindrome word)

actions_2_2 = allOf each even (2, 4, 6) -- True

actions_2_3 = maximumOf folded [(2, "I'll"), (3, "Be"), (1, "Back")] -- Just (3, "Be")

actions_2_4 = sumOf each (1, 2) -- 3

actions_3_1 = maximumByOf
  (folding words)
  (compare `on` (length . filter (`elem` ("aeiou" :: String))))
  "Do or do not, there is no try." -- Just "there"

actions_3_2 = reverse $ foldMapOf folded id ["a", "b", "c"] -- "cba"
actions_3_2' = foldByOf folded (flip (++)) "" ["a", "b", "c"]

actions_3_3 = folds_6_4_7_1 -- already solved in a previous BONUS
actions_3_4 = folds_6_4_7_2 -- already solved in a previous BONUS

------------------------ 6.4 higher-order folds --------------------------------------

ho_1_1 = ("Here's looking at you, kid" :: String) ^.. dropping 7 folded -- "looking at you, kid"

ho_1_2 =
  let xs = ["My Precious", "Hakuna Matata", "No problemo"] :: [String]
  in  xs ^.. folded . taking 1 worded -- ["My","Hakuna","No"]

ho_1_3 =
  let xs = ["My Precious", "Hakuna Matata", "No problemo"] :: [String]
  in  xs ^.. taking 2 (folded . folded) -- "My" --> (folded . worded) works too

ho_1_4 =
  let xs = ["My Precious", "Hakuna Matata", "No problemo"] :: [String]
  in  xs ^.. folded . takingWhile (/= ' ') folded -- "MyHakunaNo" --> could use 'worded' here as well

ho_1_5 = sumOf (taking 2 each) (10, 50, 100) -- 60

ho_1_6 =
  let xs = ("stressed", "guns", "evil")
  in  xs ^.. backwards each -- ["evil","guns","stressed"]

ho_1_7 =
  let xs = ("stressed", "guns", "evil")
  in  xs ^.. backwards each . to reverse -- ["live","snug","desserts"]

ho_1_8 = ("blink182 k9 blazeit420" :: String) ^.. worded . droppingWhile isAlpha folded -- "1829420"

sample :: [Int]
sample = [-10, -5, 4, 3, 8, 6, -2, 3, -5, -7]

-- number of days until temperatur above zero
ho_2_1 = lengthOf (takingWhile (<= 0) folded) sample -- 2

-- the warmest it got in the first four days
ho_2_2 = maximumOf (taking 4 folded) sample -- Just 4

-- temperature on the day after ho_2_2
ho_2_3 = sample ^? dropping 1 (droppingWhile (/= 4) folded) -- Just 3

-- consecutive below-zero days at the end of the sample... (what's the END?, probably after the 4th day)
ho_2_4 = lengthOf (to reverse . takingWhile (<0) folded) sample -- 2
ho_2_4' = lengthOf (takingWhile (<0) (backwards folded)) sample -- from the book

-- temperatures from first thaw until the next freeze
ho_2_5 = sample ^.. takingWhile (>0) (droppingWhile (<0) folded) -- [4,3,8,6]

-- BONUS: temperatures between the FIRST thaw and the FINAL freeze
ho_2_6 = sample ^.. backwards (droppingWhile (< 0) (backwards (droppingWhile (< 0) folded))) -- [4,3,8,6,-2,3]

-- Generalization of the bonus function
trimmingWhile :: (a -> Bool) -> Fold s a -> Fold s a
trimmingWhile p = backwards . droppingWhile p . backwards . droppingWhile p

ho_2_6' = sample ^.. trimmingWhile (<0) folded
