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
reorganizeRow :: Schema -> VerifiedRow -> VerifiedRow
reorganizeRow schema (VerifiedRow row) = undefined

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

schemaColInRow :: ColumnSchema -> RowWithNames -> Bool
schemaColInRow scol = any (checkSchemaType scol) 

allFieldsMatch :: Schema -> UnverifiedRow -> Bool
allFieldsMatch schema (UnverifiedRow row) = 
  all (rowcolInSchema schema) row

conditions :: [Schema -> UnverifiedRow -> Bool]
conditions = [
      hasPK
    , allFieldsMatch
  ]

  

schemaRowDiff :: NamedRow r => Schema -> r -> (Schema, r)
schemaRowDiff schema row = 
  let missingFields = filter (\sc -> not $ any (checkSchemaType sc) (unwrap row)) schema
      invalidFields = filter (not . rowcolInSchema (schema \\ missingFields)) $ unwrap row
  in (missingFields, wrap invalidFields)
  


fillNullFields :: Schema -> UnverifiedRow -> Maybe VerifiedRow
fillNullFields missing row = undefined


formatRow :: Schema -> UnverifiedRow -> Maybe VerifiedRow
formatRow schema row = do 
  let (missing, UnverifiedRow invalid) = schemaRowDiff schema row

  if null invalid then do
    filledSchema <- fillNullFields missing row
    Just $ reorganizeRow schema filledSchema
  else
    Nothing

verifyRow :: Schema -> UnverifiedRow -> Maybe VerifiedRow
verifyRow schema row = 
  if all (\f -> f schema row) conditions then
    -- fillNullFields schema row
    undefined
  else
    Nothing
