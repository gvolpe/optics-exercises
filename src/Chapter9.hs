{-# LANGUAGE RankNTypes, TemplateHaskell #-}

-- Prisms
module Chapter9 where

import           Control.Lens
import           Control.Monad                 ( guard )
import           Data.Char                     ( toUpper )
import           Data.Foldable                 ( traverse_ )
import           Data.List                     ( stripPrefix )
import           Data.Maybe                    ( listToMaybe )
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

------------------------------- prisms laws -----------------------------------

_Contains :: forall a. Ord a => a -> Prism' (S.Set a) (S.Set a)
_Contains x = prism' (S.insert x) match where
  match xs = S.filter (/= x) xs <$ guard (S.member x xs)

laws_1_1 = S.fromList [1, 2, 3] ^? _Contains 2 -- Just (fromList [1,3])

laws_1_2 = S.fromList [1, 2, 3] ^? _Contains 10 -- Nothing

laws_1_3 = _Contains 10 # S.fromList [1, 2, 3] -- fromList [1,2,3,10]

laws_1_4 = _Contains 2 # S.fromList [1, 2, 3] -- fromList [1,2,3]

-- False: because it filters values out.
containsLaw1 = preview (_Contains 2) (_Contains 2 # S.fromList [1,2,3]) == Just (S.fromList [1,2,3])

_Singleton :: forall a. Prism' [a] a
_Singleton = prism' (: []) listToMaybe

singletonLaw1 = preview _Singleton (_Singleton # 3) == Just 3

-- False: because previewing takes only the head of a list (though, the book says it's lawful???)
-- it seems lawful only when using singleton lists, e.g. [3]
singletonLaw2 =
  let s      = [1,2,3]
      Just a = preview _Singleton s
      s'     = _Singleton # a
  in  s == s'

-- any prism that does some formatting easily breaks the first law, let's prove it
_Format :: Prism' String String
_Format = prism' (fmap toUpper) Just

-- False: we get Just "FUTURAMA" instead
formatLaw1 = preview _Format (_Format # "Futurama") == Just "Futurama"
