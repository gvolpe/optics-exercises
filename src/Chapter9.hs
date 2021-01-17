{-# LANGUAGE TemplateHaskell #-}

-- Prisms
module Chapter9 where

import           Control.Lens
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
