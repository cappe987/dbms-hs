module Schema where

import Prelude as P
import Data.Int
import Data.Hashable

data SchemaType = 
    SInt32
  | SVarchar Int
  deriving Show

type Schema = [SchemaType]

getRowsize :: Schema -> Int
getRowsize [] = 0
getRowsize (SInt32:xs) = 4 + getRowsize xs
getRowsize (SVarchar n:xs) = n + getRowsize xs

getPK :: Schema -> Row -> RowValue
getPK schema = P.head 

checkSchemaType :: SchemaType -> RowValue -> Bool
checkSchemaType SInt32       (RInt32  _) = True
checkSchemaType (SVarchar _) (RString _) = True
checkSchemaType _ _ = False

schemaMatches :: Schema -> Row -> Bool
schemaMatches ss rs = and $ P.zipWith checkSchemaType ss rs

data RowValue = 
    RInt32 Int32
  | RString String
  deriving Show

type Row = [RowValue]



instance Hashable RowValue where
  hashWithSalt salt (RInt32 i ) = hashWithSalt salt i
  hashWithSalt salt (RString s) = hashWithSalt salt s

  hash (RInt32 i ) = hash i
  hash (RString s) = hash s




data TableDetails = TableDetails {
    schema    :: Schema
  , rowsize   :: Int
  , tablename :: String
  , primesize :: Int32
}


