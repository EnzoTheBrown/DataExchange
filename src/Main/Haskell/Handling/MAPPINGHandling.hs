module Main.Haskell.Handling.MAPPINGHandling where
import Main.Haskell.DataStructure
import Main.Haskell.MyMap
import Main.Haskell.Parse
import Data.Map (Map)
import qualified Data.Map as Map


-- check if the body of a rule contains NEG or not
areNeg :: [[String]] -> Bool
areNeg [] = True
areNeg ((x:xs):xss) =
  isNeg x /= 1 && areNeg xss

-- check if a datalog program is positiv or not
positive :: MAPPING -> Bool
positive (MAPPING []) = True
positive (MAPPING ((head, body):xs)) =
  areNeg body && positive (MAPPING xs)

---------------------------------------------------------------

recursive_ :: [String] -> [[String]] -> Bool
recursive_ _ [] = False
recursive_ (x:xs) ((y:ys):yss) =
  (litteral x)== (litteral y) || recursive_ (x:xs) yss


recursive :: MAPPING -> Bool
recursive (MAPPING []) = False
recursive (MAPPING ((head, body):xs))=
  (recursive_ head body) || recursive (MAPPING xs)

----------------------------------------------------------------
--          MAPPING handling
----------------------------------------------------------------

containsNEG :: [[String]] -> Bool
containsNEG [] = False
containsNEG ((head:body):xs) = (isNeg head == 1) || containsNEG xs


sliceMapping_ :: Int -> MAPPING -> Map String Int -> [([String], [[String]])]
sliceMapping_ _ (MAPPING []) _ = []
sliceMapping_ i (MAPPING (((head:vars), body):xs)) map =
  if get map head == i
    then ((head:vars), body) : sliceMapping_ i (MAPPING xs) map
    else sliceMapping_ i (MAPPING xs) map


-- MAPPING [([head, vars], [[body, vars], [body, vars], ..)...]
sliceMapping ::  Int -> Int -> MAPPING -> Map String Int -> [MAPPING]
sliceMapping size i mapping map =
  if i > size
    then
      []
    else
      (MAPPING $sliceMapping_ i mapping map) : (sliceMapping size (i+1) mapping map)

-- Sort By name
initMapByName :: MAPPING -> Map String [([String], [[String]])]
initMapByName (MAPPING []) = initMapNameMapping
initMapByName (MAPPING (((head:vars), body):xs)) =
  setNameMapping (initMapByName (MAPPING xs)) head ((head:vars), body)

--

getPositive :: [[([String], [[String]])]] -> String
getPositive [] = ""
getPositive (x:xs) =
  if positive (MAPPING x) && not (recursive (MAPPING x))
    then (fst (x!!0))!!0
    else getPositive xs

getRecursive :: [[([String], [[String]])]] -> String
getRecursive [] = ""
getRecursive (x:xs) =
  if recursive (MAPPING x)
    then (fst (x!!0))!!0
    else getRecursive xs

getNegative :: [[([String], [[String]])]] -> String
getNegative [] = ""
getNegative (x:xs) =
  if not $positive (MAPPING x)
    then (fst (x!!0))!!0
    else getNegative xs

getNEG_ [] = ""
getNEG_ (x:xs) =
  if isNeg (x!!0) == 1
    then litteral(x!!0)
    else getNEG_ xs

getNEG :: [([String], [[String]])] -> String
getNEG [] = ""
getNEG ((h, body):xs) =
  getNEG_ body ++ getNEG xs

getNegated :: [[([String], [[String]])]] -> String
getNegated [] = ""
getNegated (x:xs) =
  if not $positive (MAPPING x)
    then (fst (x!!0))!!0
    else getNegative xs


characterize :: MAPPING -> String
characterize mapping =
  if not (recursive mapping)
    then if positive mapping
         then "positive"
         else "semipositive"
    else "stratified"

