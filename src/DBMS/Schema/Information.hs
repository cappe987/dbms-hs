module DBMS.Schema.Information 
  ( getRowsize
  , removeColNames
  , getEssentialColumns
  )
  where

import Prelude as P
import Data.Int
import Data.List

import DBMS.Schema.Types


-- +1 on each for the nullbyte. For now the null byte will be there even if it
-- can't be null.
getRowsize :: Schema -> Int32
getRowsize [] = 0
getRowsize (ColumnSchema{typeof=SInt32, info=_}:xs) = 
  1 + 4 + getRowsize xs
getRowsize (ColumnSchema{typeof=SVarchar n}:xs)     = 
  1 + n + getRowsize xs
-- getRowsize (SBool    _ _:xs) = 1 + getRowsize xs


removeColNames :: NamedRow a => a -> Row
removeColNames = P.map value . unwrap






essentialColumns :: [RowProperty]
essentialColumns = [PrimaryKey, NotNull]

hasEssential :: [RowProperty] -> Bool
hasEssential = not . null . intersect essentialColumns

getEssentialColumns :: Schema -> [ColumnSchema] 
getEssentialColumns = P.filter (hasEssential . properties . info)
