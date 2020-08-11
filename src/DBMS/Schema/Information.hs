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
getRowsize (ColumnSchema{schematype=SInt32}:xs) = 
  1 + 4 + getRowsize xs
getRowsize (ColumnSchema{schematype=SVarchar n}:xs)     = 
  1 + n + getRowsize xs
-- getRowsize (SBool    _ _:xs) = 1 + getRowsize xs


removeColNames :: NamedRow a => a -> Row
removeColNames = P.map coldata . unwrap






essentialColumns :: [RowProperty]
essentialColumns = [PrimaryKey, NotNull]

hasEssential :: [RowProperty] -> Bool
hasEssential = not . null . intersect essentialColumns

getEssentialColumns :: Schema -> [ColumnSchema] 
getEssentialColumns = P.filter (hasEssential . properties)


-- ----- Schema/Row type verification -----

colTypeMatches :: ColType -> ColValue -> Bool
colTypeMatches SInt32       (VInt32  _) = True
colTypeMatches (SVarchar _) (VString _) = True
colTypeMatches _ _ = False

checkSchemaType :: ColumnSchema -> NamedColumn -> Bool
checkSchemaType cschema col = 
  let typeMatches = colTypeMatches (schematype cschema) (coldata col)
      nameMatches = name cschema == colname col
  in typeMatches && nameMatches

rowcolInSchema :: Schema -> NamedColumn -> Bool
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

schemaTypeToNullvalue :: ColType -> ColValue
schemaTypeToNullvalue SInt32       = VInt32 Nothing 
schemaTypeToNullvalue (SVarchar _) = VString Nothing 

tryGetDefault :: [RowProperty] -> Maybe ColValue
tryGetDefault (Default x:xs) = Just x
tryGetDefault (_:xs)         = tryGetDefault xs
tryGetDefault []             = Nothing

getDefaultOrNull :: ColumnSchema -> NamedColumn
getDefaultOrNull colschema = do
  let props      = properties colschema
      columnName = name       colschema
  case tryGetDefault props of
    Just defaultVal -> NamedColumn {colname=columnName, coldata=defaultVal}
    Nothing -> 
      let nulltype = schemaTypeToNullvalue (schematype colschema)
      in NamedColumn {colname=name colschema, coldata=nulltype}

