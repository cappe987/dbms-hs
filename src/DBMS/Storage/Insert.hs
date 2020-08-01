module DBMS.Storage.Insert where

import Prelude as P
import System.IO

import Data.ByteString
import Data.ByteString.Char8 as C8
import Data.Int
import Data.Function
import Data.Hashable

import Control.Monad.Trans.State as ST

import DBMS.Storage.Schema
import DBMS.Storage.Encoder
import DBMS.Storage.Decoder
import DBMS.Storage.MemoryBlock
import DBMS.Storage.Constants
import DBMS.Storage.Hashtable

-- Calculates if one more row fits in the current block
fitsInBlock :: Int -> Int -> Bool
fitsInBlock schemasize amount = 
  fromIntegral actualsize - (schemasize * amount) > schemasize

findOrMakeBlock :: TableDetails -> Handle -> Row -> Int32 -> IO ()
findOrMakeBlock details hdl row index = do
  let currblockindex = toInteger $ index * fromIntegral blocksize
  hSeek hdl AbsoluteSeek currblockindex
  bs <- hGet hdl blocksize -- Read block
  let pointer = getPointer bs

  -- print "BYTESTRING:"
  -- print bs
  -- print "POINTER"
  -- print pointer

  if pointer == nullpointer then 

    -- Doesn't point anywhere. Try to fit in this block.
    -- print $ rowsize details 
    -- print $ getBlockRowcount bs
    if fitsInBlock (rowsize details) (getBlockRowcount bs) then do
      -- return (index, Just bs <> encodeRow (schema details) row)
      -- print "FITS. WRITE LINE:"
      hSeek hdl AbsoluteSeek currblockindex
      hPut hdl $ appendRow details bs row

    else do
      -- Create new block and make the previous block point towards it.
      -- print "DOESN'T FIT"

      hSeek hdl SeekFromEnd 0
      end <- fromIntegral <$> hTell hdl :: IO Int
      -- Index of the new block. End of the file 
      let nextindex = fromIntegral $ end `div` blocksize 

      -- hPut hdl $ P.head $ createBlocks (schema details) [newrow]
      hPut hdl $ encodeBlock details [row] nullpointer

      -- Edit previous block pointer
      hSeek hdl AbsoluteSeek currblockindex -- Seeks back to previous block
      hPut hdl (replacePointer nextindex bs) -- Updates pointer for previous block

      return ()

  else
    -- Points somewhere. Follow it.
    findOrMakeBlock details hdl row pointer




-- NOTE: Handle case when trying to append to empty block
-- If lookup in hashtable fails. Create new block at the end of the file
insertRow :: TableDetails -> Row -> IO Bool
insertRow details row = do 
  let bsrow = encodeRow (schema details) row 
  -- print "ROW BEING INSERTED"
  -- print bsrow
  hdl <- openFile (tablename details ++ ".bin") ReadWriteMode 
  -- Remove the first fromIntgral when using `getActualPosition`
  let hashindex = fromIntegral $ hash (getPK (schema details) row) `mod` fromIntegral (primesize details)

  result <- findOrMakeBlock details hdl row hashindex

  hClose hdl
  return True