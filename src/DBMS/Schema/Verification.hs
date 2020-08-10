module DBMS.Schema.Verification 
  ( getNamedColumn
  , getColByName
  , tryGetPK
  , getPK
  , verifyInsertRow

  -- , allFieldsMatch
  -- , hasPK
  )
  
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

-- hasPK :: Schema -> UnverifiedRow -> Bool
-- hasPK schema row = isJust $ tryGetPK schema row

getPK :: Schema -> VerifiedRow -> NamedColValue
getPK schema row = 
  fromJust $ tryGetPK schema row




getColumn :: ColumnSchema -> UnverifiedRow -> NamedColValue
getColumn colschema (UnverifiedRow row) = 
  let columnName = name $ info colschema
  in fromJust $ find (\col -> colname col == columnName) row

-- Match row data with the schema order 
-- Requires all fields to exist
reorganizeRow :: Schema -> UnverifiedRow -> UnverifiedRow
reorganizeRow schema row = 
  UnverifiedRow $ map (`getColumn` row) schema

-- -- Notes --
-- If a column has Default and NotNull, remove the NotNull when creating schema.
-- If Default is set to null the NotNull doesn't make sense. Else if default is
-- not null then NotNull won't make sense.


-- Assumes the row is sorted according to schema
-- allTypesMatch :: Schema -> UnverifiedRow -> Bool
-- allTypesMatch schema (UnverifiedRow row) = 
--   go schema row
--   where
--     go (s:ss) (c:rs) = checkSchemaType s c && go ss rs
--     go [] (_:_)      = False
--     go (_:_) []      = True -- Shouldn't happend if it's been sorted and filled though, but just in case. If schema is longer than input then everything is alright.
--     go [] []         = True

-- allFieldsMatch :: Schema -> UnverifiedRow -> Either String UnverifiedRow
-- allFieldsMatch schema row = 
--   if all (rowcolInSchema schema) $ unwrap row then
--     Right row
--   else
--     Left ""

-- conditions :: [Schema -> UnverifiedRow -> Bool]
-- conditions = [
--       hasPK
--     -- , allFieldsMatch
--   ]

  

-- Returns Left if an empty field is marked NotNull
fillEmptyFields :: Schema -> UnverifiedRow -> Either String UnverifiedRow
fillEmptyFields missing (UnverifiedRow row) = do
  let notnulls = -- Rows that aren't allowed to be null
        filter ((not.null.intersect [PrimaryKey, NotNull]).properties.info) missing

  if not $ null notnulls then
    Left $ "NotNull field was left empty " ++ show notnulls
  else
    Right $ UnverifiedRow $ row <> map getDefaultOrNull missing


      

validateInput :: Schema -> UnverifiedRow -> Either String (UnverifiedRow, Schema)
validateInput schema row = do
  let (missing, UnverifiedRow invalid) = schemaRowDiff schema row

  if not $ null invalid then 
    Left $ "Invalid column name(s) or type mismatch: " ++ show invalid
  else if any (elem PrimaryKey . properties . info) missing then
    Left "Missing primary key" -- True if the PK column is in the list of missing data
    -- Maybe handle this in fillEmptyFields
  else 
    Right (row, missing)

verifyInsertRow :: Schema -> UnverifiedRow -> Either String VerifiedRow
verifyInsertRow schema row = do

  (row, missing) <- validateInput schema row
  filledSchema   <- fillEmptyFields missing row
  let final = reorganizeRow schema filledSchema
  -- if allTypesMatch final 
  Right $ VerifiedRow $ unwrap final 

  -- validateInput schema row
  -- >>= uncurry (flip fillEmptyFields)
  -- >>= (Right . VerifiedRow . unwrap . reorganizeRow schema)
