module DBMS.Schema.Information where

import Prelude as P
import Data.Int

import DBMS.Schema.Types


getRowsize :: Schema -> Int32
getRowsize [] = 0
getRowsize (Column{typeof=SInt32, info=_}:xs) = 4 + getRowsize xs
getRowsize (Column{typeof=SVarchar n}:xs) = n + getRowsize xs
-- getRowsize (SBool    _ _:xs) = 1 + getRowsize xs

getPK :: Schema -> Row -> RowValue
getPK schema = P.head -- Assumes that the PK comes first.
-- getPK schema row = 
  -- let pkField = P.head $ P.dropWhile ((PrimaryKey `elem`) . properties . info) schema
  -- in undefined

checkSchemaType :: Column -> RowValue -> Bool
checkSchemaType Column{typeof=SInt32    } (RInt32  _) = True
checkSchemaType Column{typeof=SVarchar _} (RString _) = True
checkSchemaType _ _ = False

schemaMatches :: Schema -> Row -> Bool
schemaMatches ss rs = and $ P.zipWith checkSchemaType ss rs
