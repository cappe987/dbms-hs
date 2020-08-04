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
    ColumnSchema{typeof=SInt32     , info=ColumnInfo{name="user_id" , properties=[PrimaryKey]}}
  , ColumnSchema{typeof=SVarchar 10, info=ColumnInfo{name="username", properties=[          ]}}
  ]
testrow    = VerifiedRow [
              NamedColValue{colname="user_id", value=RInt32 0}, 
              NamedColValue{colname="username", value=RString "Hello"}
            ]

testrow2   = [RInt32 11, RString "World"]
testrow3   = [RInt32 1, RString "Greetings"]
testrow4   = [RInt32 12, RString "Planet"]
details    = TableDetails {schema=testschema, rowsize=getRowsize testschema, tablename="test", primesize=11}

testrow5   = VerifiedRow [
              NamedColValue{colname="user_id" , value=RInt32 2},
              NamedColValue{colname="username", value=RString "Heyo"}
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
  res <- getActualPosition details 2
  print res



  -- hClose hdl





