module DBMS.Storage.Hashtable where

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




-- Use hGet to read n bytes
-- Use -1 to indicate that a block doesn't point anywhere?
-- Keep track in main db file which prime is used for each table


-- test7 :: Int
-- test7 = hash (5 :: Int32)


primes = [11, 37, 53, 97, 193, 389, 769, 1543, 3079, 6151, 12289, 24593, 49157, 98317, 196613, 393241, 786433, 1572869, 3145739, 6291469, 12582917, 25165843, 50331653, 100663319, 201326611, 402653189, 805306457, 1610612741]



dbschema :: Schema
dbschema = [SInt32, SInt32]

-- Create a table of size of the smallest prime. 
-- Write schema and hashtable size to main db file
createTable :: TableDetails ->  IO ()
createTable details = do
  let size = primesize details

  hdl <- openFile (tablename details ++ ".bin") WriteMode

  let keymap = P.map (\i -> [RInt32 i, RInt32 0]) [0..size] :: [Row]
      bss    = createBlocks dbschema keymap
      len    = fromIntegral $ P.length bss :: Int32
      blocks = 
        P.zipWith (\i bs -> bs <> encodeInt (if i == len then 0 else i)) [1..len] bss 
  -- print blocks
  mapM_ (hPut hdl) blocks

  -- return ()



-- data HashTable = []

-- Returns the actual position that the hashindex corresponds to
getActualPosition :: TableDetails -> Int32 -> IO Int32
getActualPosition details hash = do
  let index      = hash `mod` primesize details
      -- fileindex  = index * fromIntegral (getRowsize dbschema)
      maxInBlock = fromIntegral actualsize `div` getRowsize dbschema
      blockindex = index `div` fromIntegral maxInBlock

  hdl <- openFile (tablename details ++ ".bin") ReadMode
  
  hSeek hdl AbsoluteSeek $ toInteger (blocksize * fromIntegral blockindex)

  bs <- hGet hdl blocksize
  print bs

  let (rows, _)    = decodeBlock dbschema bs
      row          = P.dropWhile (\[RInt32 hash, RInt32 _] -> hash == index) rows
      (RInt32 res) = (\[_,x] -> x) $ P.head row
  print rows
  return res

getIndexByValue :: TableDetails -> RowValue -> IO Int32
getIndexByValue details value = 
  getActualPosition details (fromIntegral $ hash value)
  
getIndex :: TableDetails -> Row -> IO Int32
getIndex details row = 
  getIndexByValue details (getPK (schema details) row)







setHashPosition :: TableDetails -> Int32 -> Int32 -> IO ()
setHashPosition details hash actual = undefined

  
-- JUST FOR TESTING PURPOSES --
readDiskBlock :: Int -> String -> Schema -> IO [Row]
readDiskBlock index filename schema = do
  hdl <- openFile filename ReadMode

  hSeek hdl AbsoluteSeek $ fromIntegral (index * blocksize)

  bs <- hGet hdl blocksize

  return $ fst $ decodeBlock schema bs

  -- return []



