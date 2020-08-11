module Main where

import Lib
-- import MemoryBlock
import DBMS.Schema.Types
import DBMS.Schema.Information
import DBMS.Storage.Encoder
import DBMS.Storage.Hashtable
import DBMS.Storage.Insert

import System.IO
import Data.ByteString

testschema = [
    ColumnSchema{schematype=SInt32     , name="user_id" , properties=[PrimaryKey]}
  , ColumnSchema{schematype=SVarchar 10, name="username", properties=[NotNull          ]}
  , ColumnSchema{schematype=SVarchar 10, name="password", properties=[                 ]}
  ]
testrow    = VerifiedRow [
              NamedColumn  {colname="user_id" , coldata=VInt32  $ Just 0} 
              , NamedColumn{colname="username", coldata=VString $ Just "Hello"}
              , NamedColumn{colname="email   ", coldata=VString $ Just "Hello"}
            ]

testrowun    = UnverifiedRow [
              -- NamedColumn{colname="password", coldata=VString $ Just "Pass"}
              NamedColumn{colname="username", coldata=VString $ Just "Hello"}
              , NamedColumn  {colname="user_id" , coldata=VInt32  $ Just 0} 
              -- , NamedColumn{colname="email   ", coldata=VString $ Just "Hello"}
            ]

-- testrow2   = [VInt32 11, VString "World"]
-- testrow3   = [VInt32 1, VString "Greetings"]
-- testrow4   = [VInt32 12, VString "Planet"]
details    = TableDetails {schema=testschema, rowsize=getRowsize testschema, tablename="test", primesize=11}

testrow5   = VerifiedRow [
              NamedColumn{colname="user_id" , coldata=VInt32 $ Just 2},
              NamedColumn{colname="username", coldata=VString Nothing}
            ]

main :: IO ()
main = do
  -- writeFile "test.bin" "\NUL\NUL\NUL\NUL"
  -- This is a temporary solution to handling the first insertion
  -- Will later be handled using hashtable
  -- Data.ByteString.writeFile "test.bin" $ encodeBlock details [testrow] 0
  -- hdl <- openFile "test.bin" ReadWriteMode
  -- hClose hdl

  createTable details

  res <- insertRow details testrow
  res <- insertRow details testrow5
  -- res <- insertRow details testrow2
  -- res <- insertRow details testrow4
  -- res <- insertRow details testrow2




  -- res <- getActualPosition details 6
  -- print res
  -- setHashPosition details 7 2
  -- print res
  -- res <- getActualPosition details 2
  print res



  -- hClose hdl





