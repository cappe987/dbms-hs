module Main where

import Lib
-- import MemoryBlock
import DBMS.Storage.Schema
import DBMS.Storage.Encoder
import DBMS.Storage.Hashtable
import DBMS.Storage.Insert

import System.IO
import Data.ByteString

testschema = [SInt32, SVarchar 10]
testrow    = [RInt32 0, RString "Hello"]
testrow2   = [RInt32 11, RString "World"]
details    = TableDetails {schema=testschema, rowsize=getRowsize testschema, tablename="test", primesize=11}

main :: IO ()
main = do
  -- writeFile "test.bin" "\NUL\NUL\NUL\NUL"
  -- This is a temporary solution to handling the first insertion
  -- Will later be handled using hashtable
  -- Data.ByteString.writeFile "test.bin" $ encodeBlock details [testrow] 0
  -- hdl <- openFile "test.bin" ReadWriteMode
  -- hClose hdl


  -- res <- insertRow details testrow2
  -- res <- insertRow details testrow2
  -- res <- insertRow details testrow2
  -- res <- insertRow details testrow2
  -- res <- insertRow details testrow2


  -- createTable details


  res <- getActualPosition details 6

  print res
  -- hClose hdl





