{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-} -- these are needed for makeWrapped
{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

-- Isos
module Chapter10 where

import           Control.Lens
import           Control.Monad                 ( guard )
import           Data.Char                     ( isUpper, toLower, toUpper )
import           Data.List                     ( intercalate, sortOn, transpose )
import           Numeric.Lens
import qualified Data.List.NonEmpty            as NEL
import qualified Data.Map                      as M
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

------------------------------- projecting isos -----------------------------------

toYamlList :: [String] -> String
toYamlList xs = "- " <> intercalate "\n- " xs

pro_1_1 = putStrLn $ toYamlList ["Milk", "Eggs","Flour"]

shoppingList = ["Milk", "Eggs","Flour"] :: [T.Text]

strShoppingList = shoppingList ^. mapping unpacked :: [String]

pro_1_2 = putStrLn $ toYamlList strShoppingList

pro_1_3 = putStrLn $ shoppingList ^. mapping unpacked . to toYamlList

pro_1_4 = traverseOf_ (mapping unpacked . to toYamlList) putStrLn shoppingList

textToYamlList :: [T.Text] -> T.Text
textToYamlList = toYamlList ^. dimapping (mapping unpacked) packed

textToYamlList' :: [T.Text] -> T.Text
textToYamlList' = T.pack . toYamlList . fmap T.unpack

-- exercises

projected_1_1 = ("Beauty", "Age") ^. mapping reversed . swapped :: (String, String) -- ("egA","Beauty")

projected_1_2 = [True, False, True] ^. mapping (involuted not) -- [False,True,False]

projected_1_3 = [True, False, True] & mapping (involuted not) %~ filter id -- [False]

projected_1_4 = (show ^. mapping reversed) 1234 -- "4321"

intNot :: Int -> Int
intNot = not ^. dimapping enum (from enum)

intNot' :: Int -> Int
intNot' = enum %~ not

-- intNot 0 -- 1
-- intNot 1 -- 0
-- intNot 2 -- Exception: Prelude.Enum.Bool.toEnum: bad argument

------------------------------- isos and newtypes -----------------------------------

newtype Email = Email String deriving (Show)
newtype UserID = UserID String deriving (Show)

newts_1 :: Email
newts_1 = over coerced (reverse :: String -> String) (Email "joe@example.com") -- Email "moc.elpmaxe@eoj"

newts_2 = Email "joe@example.com" & (coerced :: Iso' Email String) . traversed %~ toUpper -- Email "JOE@EXAMPLE.COM"

email :: Iso' Email String
email = coerced

newts_3 = Email "joe@example.com" & email . traversed %~ toUpper -- Email "JOE@EXAMPLE.COM"

newtype Foo = Foo {_foo :: String} deriving (Show)
makeLenses ''Foo
makeWrapped ''Foo

newts_4 = Foo "bar" & foo . traversed %~ toUpper -- Foo {_foo = "BAR"}

newts_5 = Foo "joe@example.com" & _Wrapped' %~ reverse -- Foo {_foo = "moc.elpmaxe@eoj"}

newts_6 = Foo "joe@example.com" & _Wrapping' Foo %~ reverse -- Foo {_foo = "moc.elpmaxe@eoj"}

------------------------------- isos laws ---------------------------------

mapList :: Ord k => Iso' (M.Map k v) [(k, v)]
mapList = iso M.toList M.fromList

-- fails on duplicate keys
laws_1_1 = view (from mapList . mapList) [(1, "a"), (1, "b")] -- [(1,"b")]

nonEmptyList :: Iso [a] [b] (Maybe (NEL.NonEmpty a)) (Maybe (NEL.NonEmpty b))
nonEmptyList = iso NEL.nonEmpty (maybe [] NEL.toList)

sorted :: (Ord a) => Iso' [a] [(Int, a)]
sorted = iso to' from' where
  to'   = sortOn snd . zip [0..]
  from' = (snd <$>) . sortOn fst

laws_2_1 = view (sorted . from sorted) "foo" -- "foo"
laws_2_2 = [(22, "foo"), (33, "bar")] ^. from sorted . sorted -- [(1,"bar"),(0,"foo")] NOPE
