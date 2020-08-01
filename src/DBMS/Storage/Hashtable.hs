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


-- Create a table of size of the smallest prime. 
-- Write schema and hashtable size to main db file
createTable :: String -> Schema ->  IO ()
createTable name schema = undefined



data HashTable -- Temporary type

-- Returns the actual position that the hashindex corresponds to
getActualPosition :: HashTable -> Int -> Int32
getActualPosition ht hash = undefined



  
-- JUST FOR TESTING PURPOSES --
readDiskBlock :: Int -> String -> Schema -> IO [Row]
readDiskBlock index filename schema = do
  hdl <- openFile filename ReadMode

  hSeek hdl AbsoluteSeek $ fromIntegral (index * blocksize)

  bs <- hGet hdl blocksize

  return $ fst $ decodeBlock schema bs

  -- return []



