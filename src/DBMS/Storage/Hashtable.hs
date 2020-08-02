module DBMS.Storage.Hashtable where

import Prelude as P
import System.IO

import Data.ByteString
-- import Data.ByteString.Char8 as C8
import Data.Int
import Data.Function
import Data.Hashable


import DBMS.Storage.Schema
import DBMS.Storage.Encoder
import DBMS.Storage.Decoder
import DBMS.Storage.MemoryBlock
import DBMS.Storage.Constants




-- Use hGet to read n bytes
-- Use -1 to indicate that a block doesn't point anywhere?
-- Keep track in main db file which prime is used for each table


-- test7 :: Int
-- test7 = hash (5 :: Int32)


primes = [11, 37, 53, 97, 193, 389, 769, 1543, 3079, 6151, 12289, 24593, 49157, 98317, 196613, 393241, 786433, 1572869, 3145739, 6291469, 12582917, 25165843, 50331653, 100663319, 201326611, 402653189, 805306457, 1610612741]



htableschema :: Schema
htableschema = [SInt32, SInt32]

htableRowsize :: Int32
htableRowsize = fromIntegral $ getRowsize htableschema

-- Create a table of size of the smallest prime. 
-- Write schema and hashtable size to main db file
createTable :: TableDetails ->  IO ()
createTable details = do
  let size = primesize details

  hdl <- openFile (tablename details ++ ".bin") WriteMode

  let keymap = P.map (\i -> [RInt32 i, RInt32 0]) [0..size] :: [Row]
      bss    = createBlocks htableschema keymap
      len    = fromIntegral $ P.length bss :: Int32
      blocks = 
        P.zipWith (\i bs -> bs <> encodeInt (if i == len then 0 else i)) [1..len] bss 
  -- print blocks
  mapM_ (hPut hdl) blocks
  hClose hdl
  -- return ()

getPositionInHashtable :: Int32 -> Integer
getPositionInHashtable hashindex = 
    hashindex `div` fromIntegral (fromIntegral actualsize `div` htableRowsize)
    & (* fromIntegral blocksize)
    & toInteger
-- data HashTable = []

-- Returns the actual position that the hashindex corresponds to
-- Fetching the index requires opening of the table's file
getActualPosition :: TableDetails -> Int32 -> IO Int32
getActualPosition details hash = do
  let index       = hash `mod` primesize details
      actualindex = getPositionInHashtable index

  hdl <- openFile (tablename details ++ ".bin") ReadMode
  
  hSeek hdl AbsoluteSeek actualindex

  bs <- hGet hdl blocksize

  let (rows, _)    = decodeBlock htableschema bs
      row          = P.dropWhile (\[RInt32 hash, RInt32 _] -> hash /= index) rows
      (RInt32 res) = (\[_,x] -> x) $ P.head row

  hClose hdl
  return res

getIndexByValue :: TableDetails -> RowValue -> IO Int32
getIndexByValue details value = 
  getActualPosition details (fromIntegral $ hash value)
  
getIndex :: TableDetails -> Row -> IO Int32
getIndex details row = 
  getIndexByValue details (getPK (schema details) row)



updateHashpointer :: Int32 -> Int32 -> Row -> Row
updateHashpointer index p xs@[RInt32 i, RInt32 _]
  | i == index = [RInt32 i, RInt32 p]
  | otherwise = xs



-- This could be done without reading anything. Simply calculating 
-- where in the block the pointer is and overwriting those bytes.
setHashPosition :: TableDetails -> Int32 -> Int32 -> IO ()
setHashPosition details hash actual = do
  let index       = hash `mod` primesize details
      actualindex  = getPositionInHashtable index
  
  hdl <- openFile (tablename details ++ ".bin") ReadWriteMode
  -- Read the fileblock with the right index
  hSeek hdl AbsoluteSeek actualindex
  bs <- hGet hdl blocksize

  -- Decode it and update the pointer
  let (rows, pointer) = decodeBlock htableschema bs
      newrows   = P.map (updateHashpointer index actual) rows
  
  let htabledetails = 
        TableDetails{
            schema=htableschema
          , rowsize=fromIntegral htableRowsize
          , tablename="hashtable"
          , primesize=primesize details}

  -- Write it back to the same place
  hSeek hdl AbsoluteSeek actualindex
  hPut hdl (encodeBlock htabledetails newrows pointer)
  hClose hdl
  


-- JUST FOR TESTING PURPOSES --
readDiskBlock :: Int -> String -> Schema -> IO [Row]
readDiskBlock index filename schema = do
  hdl <- openFile filename ReadMode

  hSeek hdl AbsoluteSeek $ fromIntegral (index * blocksize)

  bs <- hGet hdl blocksize

  return $ fst $ decodeBlock schema bs

  -- return []



