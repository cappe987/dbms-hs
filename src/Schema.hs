module Schema where

import Data.Int

data SchemaType = 
    SInt32
  | SVarchar Int
  deriving Show

type Schema = [SchemaType]

rowsize :: Schema -> Int
rowsize [] = 0
rowsize (SInt32:xs) = 4 + rowsize xs
rowsize (SVarchar n:xs) = n + rowsize xs



data RowValue = 
    RInt32 Int32
  | RString String
  deriving Show

type Row = [RowValue]