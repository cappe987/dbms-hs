module DBMS.Schema.Types where

import Prelude as P
import Data.Int
import Data.Hashable



-- Rows with data
-- Add row name so it can be matched up with a column from schema?
-- Maybe wrap ColValue in another type so reading can be done without
-- assigning column names? Assign names once all the data has been read 
-- and picked out, for better performance.


data ColValue = 
    RInt32  (Maybe Int32)
  | RString (Maybe String) 
  deriving (Show, Eq)
 
instance Hashable ColValue where
  -- hashWithSalt is not planned to be used but is 
  -- required implementation for Hashable .
  hashWithSalt salt (RInt32  i) = hashWithSalt salt i
  hashWithSalt salt (RString s) = hashWithSalt salt s

  hash (RInt32  (Just i)) = hash i
  hash (RInt32  Nothing ) = error "Can't hash NULL value"
  hash (RString (Just s)) = hash s
  hash (RString Nothing ) = error "Can't hash NULL value"

type Row = [ColValue]

data NamedColValue = NamedColValue {
    colname  :: String
  , value    :: ColValue
  } deriving Show

type RowWithNames = [NamedColValue]



-- Schema metadata
data RowProperty = 
    PrimaryKey
  | ForeignKey String
  | NotNull 
  | Default ColValue
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
data ColumnSchema = ColumnSchema {
      typeof :: DataTypes
    , info   :: ColumnInfo
  } deriving (Show)

type Schema = [ColumnSchema]




-- Table metadata
data TableDetails = TableDetails {
    schema    :: Schema
  , rowsize   :: Int32
  , tablename :: String
  , primesize :: Int32
} deriving Show


-- Perhaps wrap Verified/Unverified into a typeclass for functions 
-- that can take both.

class NamedRow a where
  unwrap :: a -> RowWithNames


newtype UnverifiedRow = 
  UnverifiedRow RowWithNames 
  deriving Show

newtype VerifiedRow   = 
  VerifiedRow RowWithNames 
  deriving Show

instance NamedRow VerifiedRow where
  unwrap (VerifiedRow row) = row

instance NamedRow UnverifiedRow where
  unwrap (UnverifiedRow row) = row