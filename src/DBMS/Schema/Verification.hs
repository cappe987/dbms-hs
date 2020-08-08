module DBMS.Schema.Verification 
  -- ( tryGetPK
  -- , getPK
  -- , getNamedColumn
  -- , getColByName
  -- , verifyRow

  -- -- , allFieldsMatch
  -- -- , hasPK
  -- )
  
  where


-- import Prelude as P
import Data.Maybe
import Data.List

import DBMS.Schema.Types
import DBMS.Schema.Information

getPKColumn :: Schema -> Maybe ColumnSchema
getPKColumn schema = 
  case filter ((PrimaryKey `elem`) . properties . info) schema of
    []  -> Nothing
    [x] -> Just x
    xs  -> fail ("Too many pk: " ++ show xs ++ " in schema " ++ show schema)


getColByName :: NamedRow r => String -> r -> Maybe NamedColValue
getColByName s row = 
  case filter ((== s) . colname) (unwrap row) of
    []  -> Nothing
    [x] -> Just x 
    xs  -> fail ("Multiple columns with same name: " ++ show xs)


getNamedColumn :: NamedRow r => ColumnSchema -> r -> Maybe NamedColValue
getNamedColumn col = getColByName (name $ info col) 




tryGetPK :: NamedRow r => Schema -> r -> Maybe NamedColValue
tryGetPK schema row = 
  getPKColumn schema >>= flip getNamedColumn row

hasPK :: Schema -> UnverifiedRow -> Bool
hasPK schema row = isJust $ tryGetPK schema row

getPK :: Schema -> VerifiedRow -> NamedColValue
getPK schema row = 
  fromJust $ tryGetPK schema row



-- Match row data with the schema order
reorganizeRow :: Schema -> UnverifiedRow -> UnverifiedRow
reorganizeRow schema row = undefined

-- -- Notes --
-- If a column has Default and NotNull, remove the NotNull when creating schema.
-- If Default is set to null the NotNull doesn't make sense. Else if default is
-- not null then NotNull won't make sense.


colTypeMatches :: DataTypes -> ColValue -> Bool
colTypeMatches SInt32       (RInt32  _) = True
colTypeMatches (SVarchar _) (RString _) = True
colTypeMatches _ _ = False

checkSchemaType :: ColumnSchema -> NamedColValue -> Bool
checkSchemaType cschema col = 
  let typeMatches = colTypeMatches (typeof cschema) (value col)
      nameMatches = name (info cschema) == colname col
  in typeMatches && nameMatches

-- schemaMatches :: Schema -> Row -> Bool
-- schemaMatches ss rs = and $ P.zipWith checkSchemaType ss rs

rowcolInSchema :: Schema -> NamedColValue -> Bool
rowcolInSchema schema col = any (`checkSchemaType` col) schema

schemaColInRow :: ColumnSchema -> UnverifiedRow -> Bool
schemaColInRow scol = any (checkSchemaType scol) . unwrap

allFieldsMatch :: Schema -> UnverifiedRow -> Bool
allFieldsMatch schema (UnverifiedRow row) = 
  all (rowcolInSchema schema) row

conditions :: [Schema -> UnverifiedRow -> Bool]
conditions = [
      hasPK
    -- , allFieldsMatch
  ]

  

schemaRowDiff :: NamedRow r => Schema -> r -> (Schema, r)
schemaRowDiff schema row = 
  let missingFields = filter (\sc -> not $ any (checkSchemaType sc) (unwrap row)) schema
      invalidFields = filter (not . rowcolInSchema (schema \\ missingFields)) $ unwrap row
  in (missingFields, wrap invalidFields)
  

-- Returns Left if an empty field is marked NotNull
fillNullFields :: Schema -> UnverifiedRow -> Either String UnverifiedRow
fillNullFields missing row = 
  let notnulls = -- Rows that aren't allowed to be null
        filter ((not.null.intersect [PrimaryKey, NotNull]).properties.info) missing
  in
    if not $ null notnulls then
      Left "NotNull field was left empty"
    else
      undefined


verifyRow :: Schema -> UnverifiedRow -> Either String VerifiedRow
verifyRow schema row = do
  let (missing, UnverifiedRow invalid) = schemaRowDiff schema row

  if not $ null invalid then 
    Left "Invalid column name"
  else if any (elem PrimaryKey . properties . info) missing then
    Left "Missing primary key" -- True if the PK column is in the list of missing data
    -- Maybe handle this in fillNullFields since PK and NotNull can be treated the same
  else do
    filledSchema <- fillNullFields missing row
    Right $ VerifiedRow $ unwrap $ reorganizeRow schema filledSchema
