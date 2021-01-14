{-# LANGUAGE OverloadedStrings #-}

-- Indexable Structures
module Chapter8 where

import           Control.Lens
import qualified Data.Map                      as M
import qualified Data.Set                      as S
import qualified Data.Text                     as T

is_8_1_1 = ["Larry", "Curly", "Moe"] & ix 1 .~ "Wiggly" -- ["Larry","Wiggly","Moe"]

heroesAndVillains = M.fromList [("Superman", "Lex"), ("Batman", "Joker")]

is_8_1_2 = heroesAndVillains & at "Spiderman" .~ Just "Goblin" -- M.fromList [("Batman","Joker"),("Spiderman","Goblin"),("Superman","Lex")]

is_8_1_3 = sans "Superman" heroesAndVillains -- M.fromList [("Batman","Joker")]

is_8_1_4 = S.fromList ['a', 'e', 'i', 'o', 'u']
  & at 'y' ?~ ()
  & at 'i' .~ Nothing -- S.fromList "aeouy"

-- use ix and at to go from input to output (the book fails to mention you can also use `sans`)
input = M.fromList [("candy bars", 13), ("soda", 34), ("gum", 7)]
output = M.fromList [("candy bars",13),("ice cream",5),("soda",37)]

is_8_1_5 = input & ix "soda" +~ 3 & at "ice cream" ?~ 5 & sans "gum"
