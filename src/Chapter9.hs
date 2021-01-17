{-# LANGUAGE RankNTypes, TemplateHaskell #-}

-- Prisms
module Chapter9 where

import           Control.Lens
import           Control.Monad                 ( guard )
import           Data.Foldable                 ( traverse_ )
import           Data.List                     ( stripPrefix )
import qualified Data.Map                      as M
import qualified Data.Set                      as S
import qualified Data.Text                     as T

data ContactInfo = Email String | Telephone Int | Address String String String
makePrisms ''ContactInfo

-- the following prisms are generated
-- _Email :: Prism' ContactInfo String
-- _Telephone :: Prism' ContactInfo Int
-- _Address :: Prism' ContactInfo (String, String, String)

prisms_1_1 = Right 35 & _Right +~ 5 -- Right 40

-- ["Mind","Power","Soul","Time"]
prisms_1_2 = [Just "Mind", Just "Power", Nothing, Just "Soul", Nothing, Just "Time"]
  ^.. folded . _Just

-- [Just "Mind Stone",Just "Power Stone",Nothing,Just "Soul Stone",Nothing,Just "Time Stone"]
prisms_1_3 = [Just "Mind", Just "Power", Nothing, Just "Soul", Nothing, Just "Time"]
  & traversed . _Just <>~ " Stone"

-- Left (Right False,"Eureka!")
prisms_1_4 = Left (Right True, "Eureka!") & _Left . _1 . _Right %~ not

prisms_1_5 = _Cons # ("Do",["Re", "Mi"]) -- ["Do","Re","Mi"]

prisms_1_6 = isn't (_Show :: Prism' String Int) "not an int" -- True

-- Go from input to output
input_1  = (Just 1, Nothing, Just 3)
output_1 = [1, 3]

prism_3_1 = input_1 ^.. each . _Just

input_2  = ('x', "yz")
output_2 = "xzy"

-- (#) = review
prism_3_2 = _Cons # input_2

input_3  = "do the hokey pokey"
output_3 = Left (Just (Right "do the hokey pokey"))

prism_3_3 = _Left . _Just . _Right # input_3

------------------------------- custom prisms -----------------------------------

_Just' :: Prism (Maybe a) (Maybe b) a b
_Just' = prism Just match where
  match (Just a) = Right a
  match Nothing  = Left Nothing

_Nothing' :: Prism' (Maybe a) ()
_Nothing' = prism' (const Nothing) match where
  match Nothing  = Just ()
  match (Just _) = Nothing

_Prefix :: String -> Prism' String String
_Prefix p = prism' (p <>) (stripPrefix p)

-- fizzbuzz using prisms!
_Factor :: Int -> Prism' Int Int
_Factor n = prism' (n *) (\i -> i `div` n <$ guard (i `mod` n == 0))

prismFizzBuzz :: Int -> String
prismFizzBuzz n
  | has (_Factor 3 . _Factor 5) n = "FizzBuzz"
  | has (_Factor 3) n             = "Fizz"
  | has (_Factor 5) n             = "Buzz"
  | otherwise                     = show n

runFizzBuzz :: IO ()
runFizzBuzz = traverse_ (putStrLn . prismFizzBuzz) [1..20]

--exercises
_Tail :: Prism' [a] [a]
_Tail = prism' embed match where
  match []     = Nothing
  match (x:xs) = Just xs
  embed :: [a] -> [a]
  embed = id -- this compiles but it does not pass the laws as we do not know what the head is!

_ListCons :: Prism [a] [b] (a, [a]) (b, [b])
_ListCons = prism (uncurry (:)) match where
  match :: [a] -> Either [b] (a, [a])
  match []     = Left []
  match (x:xs) = Right (x, xs)
  embed :: (b, [b]) -> [b]
  embed (x,xs) = x:xs

_Cycles :: Eq a => Int -> Prism' [a] [a]
_Cycles n = prism' embed match where
  match xs =
    let pattern = take (length xs `div` n) xs
    in  if embed pattern == xs then Just pattern else Nothing
  embed = concat . replicate n

cycles_1 = "dogdogdog" ^? _Cycles 3 -- Just "dog"
cycles_2 = "dogdogdogdog" ^? _Cycles 3 -- Nothing (it has 4 cycles, not 3)
cycles_3 = "aaa" ^? _Cycles 3 -- Just "a"
cycles_4 = "xyz" ^? _Cycles 3 -- Nothing
cycles_5 = _Cycles 3 # "dog" -- "dogdogdog"
cycles_6 = "dogdogdog" & _Cycles 3 .~ "cats" -- "catscatscats"
