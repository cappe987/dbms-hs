module DBMS.Schema.Types where

import Prelude as P
import Data.Int
import Data.Hashable



-- Rows with data
-- Add row name so it can be matched up with a column from schema?
-- Maybe wrap ColValue in another type so reading can be done without
-- assigning column names? Assign names once all the data has been read 
-- and picked out, for better performance.

-- Schema types
data ColType = 
    SInt32   
  | SVarchar Int32
  deriving (Show, Eq)
-- | SBool    [RowProperty] Bool

data ColValue = 
    VInt32  (Maybe Int32)
  | VString (Maybe String) 
  deriving (Show, Eq)
 
instance Hashable ColValue where
  -- hashWithSalt is not planned to be used but is 
  -- required implementation for Hashable .
  hashWithSalt salt (VInt32  i) = hashWithSalt salt i
  hashWithSalt salt (VString s) = hashWithSalt salt s

  hash (VInt32  (Just i)) = hash i
  hash (VInt32  Nothing ) = error "Can't hash NULL value"
  hash (VString (Just s)) = hash s
  hash (VString Nothing ) = error "Can't hash NULL value"


data Column = Column {
    typeof :: ColType
  , value  :: ColValue
}

type Row = [ColValue]

data NamedColumn = NamedColumn {
    colname  :: String
  , coldata  :: ColValue
  } deriving Show

type RowWithNames = [NamedColumn]



-- Schema metadata
data RowProperty = 
    PrimaryKey
  | ForeignKey String
  | NotNull 
  | Default ColValue
  deriving (Show, Eq)

-- data ColumnInfo = ColumnInfo {
--     name       :: String 
--   , properties :: [RowProperty]
--   } deriving (Show, Eq)





-- The schema itself
data ColumnSchema = ColumnSchema {
      schematype :: ColType
    , name       :: String
    , properties :: [RowProperty]
  } deriving (Show, Eq)

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
  -- Not sure if I'll ever use wrap
  wrap   :: RowWithNames -> a


newtype UnverifiedRow = 
  UnverifiedRow RowWithNames 
  deriving Show

newtype VerifiedRow   = 
  VerifiedRow RowWithNames 
  deriving Show

instance NamedRow VerifiedRow where
  unwrap (VerifiedRow row) = row
  wrap = VerifiedRow 

instance NamedRow UnverifiedRow where
  unwrap (UnverifiedRow row) = row
  wrap = UnverifiedRow 