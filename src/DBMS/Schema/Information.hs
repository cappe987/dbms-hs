module DBMS.Schema.Information 
  ( getRowsize
  , removeColNames
  , getEssentialColumns
  , schemaRowDiff
  , schemaTypeToNullvalue
  , tryGetDefault
  , getDefaultOrNull
  , rowcolInSchema
  , schemaColInRow
  , checkSchemaType
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


-- ----- Schema/Row type verification -----

colTypeMatches :: DataTypes -> ColValue -> Bool
colTypeMatches SInt32       (RInt32  _) = True
colTypeMatches (SVarchar _) (RString _) = True
colTypeMatches _ _ = False

checkSchemaType :: ColumnSchema -> NamedColValue -> Bool
checkSchemaType cschema col = 
  let typeMatches = colTypeMatches (typeof cschema) (value col)
      nameMatches = name (info cschema) == colname col
  in typeMatches && nameMatches

rowcolInSchema :: Schema -> NamedColValue -> Bool
rowcolInSchema schema col = any (`checkSchemaType` col) schema

schemaColInRow :: NamedRow r => ColumnSchema -> r -> Bool
schemaColInRow scol = any (checkSchemaType scol) . unwrap





-- ----- Utils -----

schemaRowDiff :: NamedRow r => Schema -> r -> (Schema, r)
schemaRowDiff schema row = 
  let missingFields = filter (\sc -> not $ any (checkSchemaType sc) (unwrap row)) schema
      invalidFields = filter (not . rowcolInSchema (schema \\ missingFields)) $ unwrap row
  in (missingFields, wrap invalidFields)
  

-- ----- Transform schema->row -----

schemaTypeToNullvalue :: DataTypes -> ColValue
schemaTypeToNullvalue SInt32       = RInt32 Nothing 
schemaTypeToNullvalue (SVarchar _) = RString Nothing 

tryGetDefault :: [RowProperty] -> Maybe ColValue
tryGetDefault (Default x:xs) = Just x
tryGetDefault (_:xs)         = tryGetDefault xs
tryGetDefault []             = Nothing

getDefaultOrNull :: ColumnSchema -> NamedColValue
getDefaultOrNull colschema = do
  let props      = properties $ info colschema
      columnName = name       $ info colschema
  case tryGetDefault props of
    Just defaultVal -> NamedColValue {colname=columnName, value=defaultVal}
    Nothing -> 
      let nulltype = schemaTypeToNullvalue (typeof colschema)
      in NamedColValue {colname=name $ info colschema, value=nulltype}

