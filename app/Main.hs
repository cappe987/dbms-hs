module Main where

import Lib
import MemoryBlock
import Schema
import Hashtable

import System.IO

main :: IO ()
main = do
  writeFile "test.bin" "\NUL\NUL\NUL\NUL"
  hdl <- openFile "test.bin" ReadWriteMode
  hClose hdl


  let testschema = [SInt32, SVarchar 10]
      testrow    = [RInt32 0, RString "Hello"]

  res <- insertRow (TableDetails {schema=testschema, rowsize=getRowsize testschema, tablename="test", primesize=11}) testrow

  print res



  -- hClose hdl





