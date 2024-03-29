module DBMS.Storage.MemoryBlock where 

import Prelude as P
import Data.ByteString
import Data.ByteString.Char8 as C8
import Data.Serialize
import Data.Int
import Data.Function
-- import Data.Either
import Data.Maybe

import DBMS.Schema.Types
import DBMS.Storage.Encoder
import DBMS.Storage.Decoder
import DBMS.Storage.Constants
-- import System.IO



{-
### Thoughts and ideas 

Automatically number and link blocks together. When serializing a memory block
if it gets too big then create a new one where the end of the first one contains
the id of the second. 

Keep separate block(s) for the hash table. 

Store schema in JSON format in the database main file, for simplicitys sake.
Have a separate file for each table. 
Use `seek` to find read and write the correct part of a file (to avoid having 
to read the whole file).

-}



bslength :: ByteString -> Int
bslength = Data.ByteString.foldl (\acc _ -> acc + 1) 0 



-- Appends a row to the bytestring and edits the counter metadata
-- Keeps the pointer. This could occur if rows are deleted from a 
-- block that points somewhere, so new rows can be added to that block.
appendRow :: TableDetails -> ByteString -> Row -> ByteString
appendRow details orig row = 
  let (rows, pointer) = decodeBlock (schema details) orig
      -- newrow          = evalState (decodeRow schema) row
  in encodeBlock details (rows ++ [row]) pointer

-- Adds a pointer to a block that doesn't have one. 
addPointer :: Int32 -> ByteString -> ByteString
addPointer p = (<> encodeInt p)

-- Updates the pointer at the end of a block bytestring
replacePointer :: Int32 -> ByteString -> ByteString
replacePointer p bs = 
  C8.reverse bs
  & C8.drop (fromIntegral pointersize)
  & C8.reverse
  & (<> encodeInt (fromIntegral p))


-- getPointer :: ByteString -> Int32
-- getPointer = decodeInt . removePaddingToPointer


-- -- getPointer = fromIntegral . evalState decodePointer
-- getPointer = evalState decodePointer

getBlockRowcount :: ByteString -> Int32
getBlockRowcount = decodeInt



-- Testing functions

-- testschema = [SInt32, SVarchar 10] 

-- test2 = 
--   let x = encode (5 :: Int32) <> encodeString "Hello"
--   in evalState ((,) <$> parseInt <*> parseVarchar 5) x

-- test3 = 
--   let x = encodeString "HelloWorld"
--   in evalState (parseVarchar 5) x


-- test4 = C8.writeFile "test.bin" (encodeString "HelloWorld")
-- test5 = C8.writeFile "test.bin" (encode (65 :: Int32))
  
-- test6 = 
--   createBlocks testschema [[RInt32 5, RString "Hello"], [RInt32 10, RString "World"]]
--   & P.map (addPointer 5)
--   & P.map (decodeBlock testschema)

