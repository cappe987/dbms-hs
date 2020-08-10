-- {-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE DeriveGeneric #-}
module DBMS.Schema.Newtypes where

import Data.Int
import Data.ByteString
import Data.Hashable

-- import DBMS.Schema.Types

data ColType = 
    SInt32 
  | SVarchar Int32
  deriving (Show, Eq)

data ColValue = 
    CInt32 (Maybe Int32)
  | CString (Maybe String)
  deriving (Show, Eq)

newtype Varchar = 
  Varchar {toString :: String}
  deriving (Show, Eq)

instance Semigroup Varchar where
  v1 <> v2 = Varchar (toString v1 <> toString v2)
instance Monoid Varchar where
  mempty = Varchar ""




-- class Hashable a => Columnable a where
class Columnable a where
  encode :: Int32 -> Maybe a -> ByteString
  decode :: Int32 -> ByteString -> Maybe a


-- Move instances to a Columnable file
instance Columnable Int32 where
  encode _ int = undefined
  decode _ bs  = undefined

instance Columnable Varchar where
  encode n string = undefined
  decode n bs     = undefined

-- encodeMaybeVarchar :: Int32 -> Maybe String -> ByteString

-- parseMaybeVarchar :: Int32 -> State ByteString (Maybe String)

-- decodeVarchar :: Int32 -> ByteString -> Maybe String
-- decodeInt :: ByteString -> Int32


-- where `a` is Columnable
data Column = Column {
    typeof :: ColType
  , value  :: ColValue
} deriving (Show)


data NamedColumn = NamedColumn {
    colname :: String
  , coldata :: Column
}

newtype Row a = Row [Column]
newtype NamedRow a = NamedRow [NamedColumn]
  --   CInt32 ColType (Maybe Int32)
  -- | CString (Maybe String)
  -- deriving (Eq)
  -- , properties :: [RowProperty]


