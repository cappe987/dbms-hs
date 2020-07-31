module Hashtable where

import Prelude as P
import System.IO

import Data.ByteString
import Data.ByteString.Char8 as C8
import Data.Int
import Data.Function
import Data.Hashable

import Control.Monad.Trans.State as ST

import Schema
import Encoder
import Decoder
import MemoryBlock




-- Use hGet to read n bytes
-- Use -1 to indicate that a block doesn't point anywhere?
-- Keep track in main db file which prime is used for each table


test7 :: Int
test7 = hash (5 :: Int32)

nullpointer :: Int
nullpointer = 0

primes = [11, 37, 53, 97, 193, 389, 769, 1543, 3079, 6151, 12289, 24593, 49157, 98317, 196613, 393241, 786433, 1572869, 3145739, 6291469, 12582917, 25165843, 50331653, 100663319, 201326611, 402653189, 805306457, 1610612741]


-- Create a table of size of the smallest prime. 
-- Write schema and hashtable size to main db file
createTable :: String -> Schema ->  IO ()
createTable name schema = undefined



data TableDetails = TableDetails {
    schema    :: Schema
  , rowsize   :: Int
  , tablename :: String
  , primesize :: Int32
}



-- Calculates if one more row fits in the current block
fitsInBlock :: Int -> Int -> Bool
fitsInBlock schemasize amount = actualsize - (schemasize * amount) > schemasize


replacePointer :: Int -> ByteString -> ByteString
replacePointer p bs = 
  C8.reverse bs
  & C8.drop pointersize
  & C8.reverse
  & (<> encodeInt (fromIntegral p))

getPointer :: ByteString -> Int
getPointer = fromIntegral . evalState decodePointer

getBlockRowcount :: ByteString -> Int
getBlockRowcount = fromIntegral . evalState decodePointer 

findOrMakeBlock :: TableDetails -> Handle -> ByteString -> Int -> IO ()
findOrMakeBlock details hdl bsrow index = do
  hSeek hdl AbsoluteSeek $ toInteger (index * blocksize)
  bs <- hGet hdl blocksize -- Read block
  let pointer = getPointer bs

  print "BYTESTRING:"
  print bs
  print "POINTER"
  print pointer

  if pointer == nullpointer then

    -- Doesn't point anywhere. Try to fit in this block.
    if fitsInBlock (rowsize details) (getBlockRowcount bs) then do
      -- return (index, Just bs <> encodeRow (schema details) row)
      print "FITS"
      let olddata = fst $ readBlock (schema details) bs
          newdata = olddata ++ [evalState (decodeRow (schema details)) bsrow]

      hPut hdl $ P.head $ createBlocks (schema details) newdata

    else do
      -- Create new block and make the previous block point towards it.
      print "DOESN'T FIT"
      let prevblockindex = toInteger $ index * blocksize

      hSeek hdl SeekFromEnd 0
      end <- hTell hdl
      -- Index of the new block. 
      -- End of the file 
      let nextindex = fromIntegral end `div` blocksize 
          newrow    = evalState (decodeRow (schema details)) bsrow

      hPut hdl $ P.head $ createBlocks (schema details) [newrow]

      -- Edit previous block pointer
      hSeek hdl AbsoluteSeek prevblockindex -- Seeks back to previous block
      hPut hdl (replacePointer nextindex bs) -- Updates pointer for previous block

      return ()

  else
    -- Points somewhere. Follow it.
    findOrMakeBlock details hdl bsrow pointer


insertRow :: TableDetails -> Row -> IO Bool
insertRow details row = do 
  let bsrow = encodeRow (schema details) row 
  print "ROW BEING INSERTED"
  print bsrow
  hdl <- openFile (tablename details ++ ".bin") ReadWriteMode 
  let hashindex = hash (getPK (schema details) row) `mod` fromIntegral (primesize details)

  result <- findOrMakeBlock details hdl bsrow hashindex

  hClose hdl
  return True

  
-- JUST FOR TESTING PURPOSES --
readDiskBlock :: Int -> String -> Schema -> IO [Row]
readDiskBlock index filename schema = do
  hdl <- openFile filename ReadMode

  hSeek hdl AbsoluteSeek $ fromIntegral (index * blocksize)

  bs <- hGet hdl blocksize

  return $ fst $ readBlock schema bs

  -- return []



