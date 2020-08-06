module DBMS.Schema.Verification 
  ( tryGetPK
  , getPK
  , getNamedColumn
  , getColByName
  , verifyRow

  -- , allFieldsMatch
  -- , hasPK
  )
  
  where


-- import Prelude as P
import Data.Maybe

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

allFieldsMatch :: Schema -> UnverifiedRow -> Bool
allFieldsMatch schema (UnverifiedRow row) = 
  all (\col -> any (`checkSchemaType` col) schema) row

conditions :: [Schema -> UnverifiedRow -> Bool]
conditions = [
      hasPK
    , allFieldsMatch
  ]


fillNullFields :: Schema -> VerifiedRow -> VerifiedRow
fillNullFields schema row = undefined

verifyRow :: Schema -> UnverifiedRow -> Maybe VerifiedRow
verifyRow schema row = 
  if all (\f -> f schema row) conditions then
    Just $ VerifiedRow $ unwrap row
  else
    Nothing
