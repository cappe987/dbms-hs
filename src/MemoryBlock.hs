module MemoryBlock where 

import Prelude as P
import Data.ByteString
import Data.ByteString.Char8 as C8
import Data.Serialize
import Data.Int
import Control.Monad.Trans.State as ST
import Data.Function
-- import Data.Either
import Data.Maybe

import Schema
import Encoder
import Decoder
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



-- type MemoryBlock a = [a]




bslength :: ByteString -> Int
bslength = Data.ByteString.foldl (\acc _ -> acc + 1) 0 




-- decodeVarchar :: Int -> String -> State ByteString String
-- decodeVarchar 0 _   = return  ""
-- decodeVarchar len s = do
--   bs <- ST.get
--   let res = decode bs :: Either String Char
--       c = fromRight '\NUL' res
--   ST.put $ Data.ByteString.drop 1 bs
--   let result 
--         | len == 1 = return $ P.reverse (c:s)
--         | otherwise = decodeVarchar (len-1) (c:s)
--   result



-- decodeVarchar len s = do
--   bs <- ST.get
--   let res = decode bs :: Either String Char
--       c = fromRight '\NUL' res
--   ST.put $ Data.ByteString.drop 1 bs
--   decodeVarchar (len-1) (c:s)



test2 = 
  let x = encode (5 :: Int32) <> encodeString "Hello"
  in evalState ((,) <$> decodeInt <*> decodeVarchar 5) x

test3 = 
  let x = encodeString "HelloWorld"
  in evalState (decodeVarchar 5) x


test4 = C8.writeFile "test.bin" (encodeString "HelloWorld")
test5 = C8.writeFile "test.bin" (encode (65 :: Int32))
-- test2 = 
--   let x = encode (5 :: Int32) <> encode "Hello"
--   in case decodeInt x of
--     Just (a, rest) ->
--       case decodeString rest of
--         Just (b, rest) -> Just (a, b, rest)
--         Nothing -> Nothing
--     Nothing -> Nothing




-- instance Serialize VarcharString where
--   put (VarcharString s) = putByteString $ C8.pack s


-- instance Serialize Schema where
--   put (Some ss is) = put ss <> put is
--   get (Some ss is) = 


-- data MemoryBlock a where
--   Block :: Serialize a => a -> MemoryBlock a

intMax :: Int32
intMax = 2147483647





-- Size of a block on disk
blocksize :: Int
-- blocksize = 4096
-- blocksize = 30
blocksize = 60

-- Points to where the next block is
-- Positioned at the 4 last bytes of a block
pointersize :: Int
pointersize = 4

-- Contains how many rows exists in a block
-- Positioned at the 4 first bytes of a block
blockmetadata :: Int
blockmetadata = 4

actualsize :: Int
actualsize = blocksize - pointersize - blockmetadata


padBytestring :: Int -> ByteString
padBytestring n = 
  P.foldl (<>) Data.ByteString.empty (P.replicate n (encode '\NUL'))

padMemoryBlock :: Int -> Int -> ByteString
padMemoryBlock rowsize rowcount = 
  padBytestring (blocksize - (blockmetadata + rowsize*rowcount + pointersize)) 



-- Creates block without the 4 pointer bytes at the end
-- Ignores any rows that don't fit
createBlock :: TableDetails -> [Row] -> ByteString
createBlock details rows = 
  let size           = rowsize details
      len            = P.length rows
      maxAmount      = actualsize `div` size 
      nrRowsToEncode = min maxAmount len
      rowsInBlock    = encodeInt (fromIntegral nrRowsToEncode)
      bs             = encodeRows (schema details) (P.take nrRowsToEncode rows)
  in rowsInBlock <> bs <> padMemoryBlock size nrRowsToEncode

-- Ignores any rows that don't fit
createBlockWPointer :: TableDetails -> [Row] -> Int32 -> ByteString
createBlockWPointer details rows pointer = 
  createBlock details rows <> encodeInt pointer

-- Appends a row to the bytestring and edits the counter metadata
-- Keeps the pointer. This could occur if rows are deleted from a 
-- block that points somewhere, so new rows can be added to that block.
appendRow :: TableDetails -> ByteString -> Row -> ByteString
appendRow details orig row = 
  let (rows, pointer) = readBlock (schema details) orig
      -- newrow          = evalState (decodeRow schema) row
  in createBlock details (rows ++ [row]) <> encodeInt pointer



-- Note: add handling for schemas that are larger 
-- than (blocksize - pointersize - blockmetadata) bytes.
-- Returns a list of ByteStrings where the last 4 bytes for pointersize are not added.
-- So the calling function can fill in the pointers or null.
createBlocks :: Schema -> [Row] -> [ByteString]
createBlocks schema [] = []
createBlocks schema xs = 
  let size           = getRowsize schema
      len            = P.length xs
      maxAmount      = (blocksize - pointersize - blockmetadata) `div` size 
      nrRowsToEncode = min maxAmount len
      rowsInBlock    = encodeInt (fromIntegral nrRowsToEncode)
      bs             = encodeRows schema (P.take nrRowsToEncode xs)
      remainingRows  = P.drop nrRowsToEncode xs
      block = rowsInBlock <> bs <> padMemoryBlock size nrRowsToEncode
  in 
    block : createBlocks schema remainingRows
    -- rest <- createBlocks schema remainingRows
    -- Just $ paddedBs : rest


addPointer :: Int32 -> ByteString -> ByteString
addPointer p = (<> encodeInt p)

readBlock :: Schema -> ByteString -> ([Row], Int32)
readBlock schema = 
  evalState ((,) <$> (decodeInt >>= decodeRows schema) <*> decodePointer)
  



testschema = [SInt32, SVarchar 10] 
  
test6 = 
  createBlocks testschema [[RInt32 5, RString "Hello"], [RInt32 10, RString "World"]]
  & P.map (addPointer 5)
  & P.map (readBlock testschema)

