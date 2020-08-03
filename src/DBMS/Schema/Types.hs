module DBMS.Schema.Types where

import Prelude as P
import Data.Int
import Data.Hashable



-- Rows with data
-- Add row name so it can be matched up with a column from schema?
-- Maybe wrap RowValue in another type so reading can be done without
-- assigning column names? Assign names once all the data has been read 
-- and picked out, for better performance.
data RowValue = 
    RInt32  Int32
  | RString String
  deriving (Show, Eq)

instance Hashable RowValue where
  -- hashWithSalt is not planned to be used but is 
  -- required implementation for Hashable .
  hashWithSalt salt (RInt32  i) = hashWithSalt salt i
  hashWithSalt salt (RString s) = hashWithSalt salt s

  hash (RInt32  i) = hash i
  hash (RString s) = hash s

type Row = [RowValue]



-- Schema metadata
data RowProperty = 
    PrimaryKey
  | ForeignKey String
  | NotNull 
  | Default RowValue
  deriving (Show, Eq)

data ColumnInfo = ColumnInfo {
    name       :: String 
  , properties :: [RowProperty]
  } deriving (Show)



-- Schema types
data DataTypes = 
    SInt32   
  | SVarchar Int32
  deriving (Show, Eq)
-- | SBool    [RowProperty] Bool


-- The schema itself
data Column = Column {
      typeof :: DataTypes
    , info   :: ColumnInfo
  } deriving (Show)

type Schema = [Column]




-- Table metadata
data TableDetails = TableDetails {
    schema    :: Schema
  , rowsize   :: Int32
  , tablename :: String
  , primesize :: Int32
} deriving Show


