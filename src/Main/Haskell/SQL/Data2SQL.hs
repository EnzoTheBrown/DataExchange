module Main.Haskell.SQL.Data2SQL where
import Main.Haskell.SQL.GenerateSQL
import Main.Haskell.Evaluation 
import Main.Haskell.DataStructure
import Main.Haskell.MyMap
import Main.Haskell.Handling.MAPPINGHandling
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map


commit :: String
commit = "\n\n"
  ++ "----------\n"
  ++ "COMMIT;"
  ++ "\n----------"
  ++ "\n\n"


handleRecursiveness :: MAPPING -> String
handleRecursiveness (MAPPING mapping) =
  createViewSqlRecursivePositive (mapping!!0) (mapping!!1)

handleNegativeness :: MAPPING -> String
handleNegativeness (MAPPING mapping) =
  createViewSqlRecursiveNegative (mapping!!0) (mapping!!1) (mapping!!2)

handleMultipleness :: MAPPING -> String
handleMultipleness (MAPPING mapping) =
  intercalate "\n\n" [createViewSql (((head ++ [last(show (elemIndex ((head:vars), body) mapping))]):vars), body) | ((head:vars), body) <- mapping ]
  ++ "\n\n "
  ++ "CREATE OR REPLACE VIEW " ++ ((fst (mapping!!0))!!0) ++ " AS \n  SELECT * FROM "
  ++ intercalate "\n UNION ALL \n SELECT * FROM " [head ++ [last (show (elemIndex ((head:vars), body) mapping))] | ((head:vars), body) <- mapping ]
  ++ "\n;"

data2Sql :: EDB -> Map String [([String], [[String]])] -> String
data2Sql edb mapMapping = do
  createTable (unifyEDB edb (EDB []))
  ++ commit
  ++ insertTable edb
  ++ commit
  ++ handleMultipleness (MAPPING (getNameMapping mapMapping (getPositive [snd x | x <- Map.toList mapMapping])))
  ++ "\n\n\n"
  ++ handleRecursiveness (MAPPING (getNameMapping mapMapping (getRecursive  [snd x | x <- Map.toList mapMapping])))
  ++ "\n\n\n"
  ++ handleNegativeness (MAPPING ((getNameMapping mapMapping (getRecursive  [snd x | x <- Map.toList mapMapping])) ++ getNameMapping mapMapping (getNegative  [snd x | x <- Map.toList mapMapping])))
  ++ commit




