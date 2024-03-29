module DBMS.Storage.Insert where

import Prelude as P
import System.IO

import Data.ByteString
-- import Data.ByteString.Char8 as C8
import Data.Int
-- import Data.Function
-- import Data.Hashable


import DBMS.Schema.Types
import DBMS.Schema.Information
import DBMS.Storage.Encoder
import DBMS.Storage.Decoder
import DBMS.Storage.MemoryBlock
import DBMS.Storage.Constants
import DBMS.Storage.Hashtable

-- Calculates if one more row fits in the current block
fitsInBlock :: Int32 -> Int32 -> Bool
fitsInBlock schemasize amount = 
  fromIntegral actualsize - (schemasize * amount) > schemasize

findOrMakeBlock :: TableDetails -> Handle -> Row -> Int32 -> IO Bool
findOrMakeBlock details hdl row index = do
  let currblockindex = toInteger $ index * fromIntegral blocksize
  hSeek hdl AbsoluteSeek currblockindex
  bs <- hGet hdl blocksize -- Read block
  let pointer = getPointer bs

  -- Check if PK exists in this block, return false if it does.

  if pointer == nullpointer then 

    -- Doesn't point anywhere. Try to fit in this block.
    if fitsInBlock (rowsize details) (getBlockRowcount bs) then do
      -- return (index, Just bs <> encodeRow (schema details) row)
      hSeek hdl AbsoluteSeek currblockindex
      hPut hdl $ appendRow details bs row
      return True

    else do
      -- Create new block and make the previous block point towards it.

      indexOfBlock <- createBlockAtEnd details hdl row

      -- Edit previous block pointer
      hSeek hdl AbsoluteSeek currblockindex -- Seeks back to previous block
      hPut hdl (replacePointer indexOfBlock bs) -- Updates pointer for previous block

      return True

  else
    -- Points somewhere. Follow it.
    findOrMakeBlock details hdl row pointer


-- Returns the new index
createBlockAtEnd :: TableDetails -> Handle -> Row -> IO Int32
createBlockAtEnd details hdl row = do
  hSeek hdl SeekFromEnd 0
  end <- fromIntegral <$> hTell hdl :: IO Int
  -- Index of the new block. End of the file 

  hPut hdl $ encodeBlock details [row] nullpointer

  return $ fromIntegral $ end `div` blocksize 


-- NOTE: Handle case when trying to append to empty block
-- If lookup in hashtable fails. Create new block at the end of the file
-- On parsing the row, map it to a RowsWithName
-- insertRow :: TableDetails -> RowWithName -> IO Bool
insertRow :: TableDetails -> VerifiedRow -> IO Bool
insertRow details rowWithNames = do 
  let row       = removeColNames rowWithNames
      bsrow     = encodeRow (schema details) row
      hashvalue = hashPK details rowWithNames

  actualindex <- getIndex details rowWithNames

  if actualindex == 0 then do
    -- create a new block at the end
    -- Can open in only WriteMode in that case
    -- No need to check if PK exists
    hdl <- openFile (tablename details ++ ".bin") ReadWriteMode 
    newindex <- createBlockAtEnd details hdl row
    hClose hdl

    setHashPosition details hashvalue newindex
    return True
  else do
    hdl <- openFile (tablename details ++ ".bin") ReadWriteMode 
    -- Check if PK exists first.
    res <- findOrMakeBlock details hdl row actualindex

    hClose hdl
    return res