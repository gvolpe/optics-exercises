{-# LANGUAGE OverloadedStrings #-}

-- Isos
module Chapter10 where

import           Control.Lens
import           Control.Monad                 ( guard )
import           Data.Char                     ( isUpper, toLower, toUpper )
import           Data.List                     ( transpose )
import           Numeric.Lens
import qualified Data.Text                     as T

-- both `packed` and `unpacked` are defined in Data.Text.Lens
packed :: Iso' String T.Text
packed = iso T.pack T.unpack

unpacked :: Iso' T.Text String
unpacked = from packed

pack1 :: T.Text
pack1 = ("Ay, caramba!" :: String) ^. packed

pack2 :: String
pack2 = packed # ("Sufferin' Succotash" :: T.Text)

pack3 :: T.Text
pack3 = ("Good grief" :: String) ^. packed

pack4 :: String
pack4 = ("Good grief" :: T.Text) ^. from packed

-- modifications
str = "Idol on a pedestal" :: String

mod1 = str & packed %~ T.replace "Idol" "Sand"
mod2 = over packed (T.replace "Idol" "Sand") str

txt = "Lorem ipsum" :: T.Text
-- We could just use `T.toUpper` instead, but this demonstrates the point:
mod3 = txt & unpacked . traversed %~ toUpper

mod4 = "Blue suede shoes" & reversed . taking 1 worded . reversed .~ "gloves" -- "Blue suede gloves"

mod5 = ("Fall","Pride") ^. swapped -- ("Pride","Fall")

mod6 = Right "Field" ^. swapped -- Left "Field"

(++?) = (++) ^. flipped
mod7 = "A" ++? "B" -- "BA"

addTuple = (+) ^. uncurried
mod8 = addTuple (1, 2) -- 3

-- a. 30 / 10 = 3   (to dividing)
-- b. 3 * 2 = 6     (to multiplying)
-- c. 6 + 1 = 7     (function +1)
-- d. 7 / 2 = 3.5   (from multiplying)
-- e. 3.5 * 10 = 35 (from dividing)
mod9 = 30 & dividing 10 . multiplying 2 +~ 1 --35

-- exercises
-- Iso
isos_1_1 = 30 ^. fahrenheit -- 86.0

-- Traversal
isos_1_2 = over reversed (take 1) [1,2,3] -- [3]

-- Prism
-- isos_1_3 = decoding Json can fail.

isos_2_1 = ("Beauty", "Age") ^. swapped -- ("Age","Beauty")

isos_2_2 = 50 ^. from (adding 10) -- 40

isos_2_3 = 0 & multiplying 4 +~ 12 -- 3.0

isos_2_4 = 0 & adding 10 . multiplying 2 .~ 24 -- 2.0

isos_2_5 = [1, 2, 3] & reversed %~ drop 1 -- [1,2]

isos_2_6 = view flipped (++) [1, 2] [3, 4] -- [3,4,2,1]

isos_2_7 = [1, 2, 3] ^. reversed -- [3,2,1]

isos_2_8 = [[1, 2, 3], [10, 20, 30]] & involuted transpose %~ drop 1 -- [[2,3],[20,30]]

isos_2_9 = (32, "Hi") & _2 . involuted (fmap switchCase) .~ ("hELLO" :: String) -- (32, "Hello")

switchCase :: Char -> Char
switchCase c = if isUpper c then toLower c else toUpper c

fahrenheit :: Iso' Double Double
fahrenheit = iso ctf ftc where
  ctf c = (c * (9/5)) + 32
  ftc f = (f - 32) / (9/5)
