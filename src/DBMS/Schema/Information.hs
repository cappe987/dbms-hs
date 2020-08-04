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


getRowsize :: Schema -> Int32
getRowsize [] = 0
getRowsize (ColumnSchema{typeof=SInt32, info=_}:xs) = 4 + getRowsize xs
getRowsize (ColumnSchema{typeof=SVarchar n}:xs) = n + getRowsize xs
-- getRowsize (SBool    _ _:xs) = 1 + getRowsize xs


removeColNames :: NamedRow a => a -> Row
removeColNames = P.map value . unwrap


checkSchemaType :: ColumnSchema -> ColValue -> Bool
checkSchemaType ColumnSchema{typeof=SInt32    } (RInt32  _) = True
checkSchemaType ColumnSchema{typeof=SVarchar _} (RString _) = True
checkSchemaType _ _ = False

schemaMatches :: Schema -> Row -> Bool
schemaMatches ss rs = and $ P.zipWith checkSchemaType ss rs




essentialColumns :: [RowProperty]
essentialColumns = [PrimaryKey, NotNull]

hasEssential :: [RowProperty] -> Bool
hasEssential = not . null . intersect essentialColumns

getEssentialColumns :: Schema -> [ColumnSchema] 
getEssentialColumns = P.filter (hasEssential . properties . info)
