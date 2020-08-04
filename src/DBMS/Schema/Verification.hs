module DBMS.Schema.Verification 
  ( tryGetPK
  , getPK
  , getNamedColumn
  , getColByName
  , verifyRow
  )
  
  where


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




tryGetPK :: Schema -> UnverifiedRow -> Maybe NamedColValue
tryGetPK schema row = 
  getPKColumn schema >>= flip getNamedColumn row

hasPK :: Schema -> UnverifiedRow -> Bool
hasPK schema row = isJust $ tryGetPK schema row

getPK :: Schema -> VerifiedRow -> NamedColValue
getPK schema (VerifiedRow row) = 
  fromJust $ tryGetPK schema (UnverifiedRow row)




-- -- Notes --
-- If a column has Default and NotNull, remove the NotNull when creating schema.
-- If Default is set to null the NotNull doesn't make sense. Else if default is
-- not null then NotNull won't make sense.

conditions :: [Schema -> UnverifiedRow -> Bool]
conditions = [
    hasPK
  ]

verifyRow :: Schema -> UnverifiedRow -> VerifiedRow
verifyRow schema row = undefined