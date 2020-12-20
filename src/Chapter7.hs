-- Traversals
module Chapter7 where

import           Control.Lens
import           Data.Char                      ( toUpper )

traversals_7_1_1 = "You get a Fold"
traversals_7_1_2 = "Lenses (and traversals, of course)"
traversals_7_1_3 = "Both Lenses & Traversals (and folds, of course)"

traversals_7_2_1 = ("Jurassic", "Park") & each .~ "N/A" -- ("N/A","N/A") -- could also use `both`

traversals_7_2_2 =
  let xs = ("Jurassic", "Park") :: (String, String)
  in  xs & both . traversed .~ 'x' -- ("xxxxxxxx","xxxx") -- could also use `both . each`:

traversals_7_2_3 =
  let xs = ("Malcolm", ["Kaylee", "Inara", "Jayne"])
  in  xs & beside id traversed %~ take 3 -- ("Mal",["Kay","Ina","Jay"])

traversals_7_2_4 =
  let xs = ("Malcolm", ["Kaylee", "Inara", "Jayne"])
  in  xs & _2 . element 1 .~ "River" -- ("Malcolm",["Kaylee","River","Jayne"])

traversals_7_2_5 =
  let xs = ["Die Another Day", "Live and Let Die", "You Only Live Twice"]
  in  xs & traversed . elementOf worded 1 . traversed .~ 'x' -- ["Die xxxxxxx Day","Live xxx Let Die","You xxxx Live Twice"]

traversals_7_2_6 = ((1, 2), (3, 4)) & both . both +~ 1 -- ((2, 3), (4, 5)) -- could also be `each . each` or `both . each`

traversals_7_2_7 = (1, (2, [3, 4])) & beside id (beside id traversed) +~ 1 -- (2, (3, [4, 5]))

-- it would also work if we replace the `traversed` after the filter by `_2`
traversals_7_2_8 =
  let xs = ((True, "Strawberries"), (False, "Blueberries"), (True, "Blackberries")) :: ((Bool, String), (Bool, String), (Bool, String))
  in  xs & each . filtered fst . traversed . taking 5 traversed %~ toUpper -- ((True, "STRAWberries"), (False, "Blueberries"), (True, "BLACKberries"))

traversals_7_2_9 =
  let xs = ((True, "Strawberries"), (False, "Blueberries"), (True, "Blackberries")) :: ((Bool, String), (Bool, String), (Bool, String))
  in  xs & each %~ snd -- ("Strawberries","Blueberries","Blackberries")
