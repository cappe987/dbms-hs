module MemoryBlock where 

import Data.ByteString
import Data.ByteString.Char8 as C8
import Data.Serialize
import Data.Int
import Control.Monad.Trans.State as ST
-- import Data.Either

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
--         | len == 1 = return $ Prelude.reverse (c:s)
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

encodeMB :: Schema -> [Row] -> Maybe ByteString
encodeMB schema = 
  Prelude.foldl (\acc r-> acc <> encodeRow schema r) (Just Data.ByteString.empty)

-- data MemoryBlock a where
--   Block :: Serialize a => a -> MemoryBlock a

intMax :: Int32
intMax = 2147483647





-- Size of a block on disk
blocksize :: Int
-- blocksize = 4096
blocksize = 30

-- Points to where the next block is
-- Positioned at the 4 last bytes of a block
pointersize :: Int
pointersize = 4

-- Contains how many rows exists in a block
-- Positioned at the 4 first bytes of a block
blockmetadata :: Int
blockmetadata = 4



padBytestring :: Int -> ByteString
padBytestring n = 
  Prelude.foldl (<>) Data.ByteString.empty (Prelude.replicate n (encode '\NUL'))

padMemoryBlock :: Int -> Int -> ByteString
padMemoryBlock rowsize rowcount = 
  padBytestring (blocksize - (blockmetadata + rowsize*rowcount + pointersize)) 

-- Note: add handling for schemas that are larger 
-- than (blocksize - pointersize - blockmetadata) bytes.
-- Returns a list of ByteStrings where the last 4 bytes for pointersize are not added.
-- So the calling function can fill in the pointers or null.
createBlocks :: Schema -> [Row] -> Maybe [ByteString]
createBlocks schema [] = Just []
createBlocks schema xs = 
  let size          = rowsize schema
      maxAmount     = (blocksize - pointersize - blockmetadata) `div` size 
      rowsInBlock   = encodeInt (fromIntegral maxAmount)
      bsMaybe       = encodeMB schema (Prelude.take maxAmount xs)
      remainingRows = Prelude.drop maxAmount xs
  in do
    bs <- bsMaybe
    let block = rowsInBlock <> bs <> padMemoryBlock size maxAmount
    
    (:) <$> Just block <*> createBlocks schema remainingRows
    -- rest <- createBlocks schema remainingRows
    -- Just $ paddedBs : rest

  
test6 = 
  createBlocks 
    [SInt32, SVarchar 7] 
    [[RInt32 5, RString "Hello"], [RInt32 10, RString "World"]]


-- test6 = undefined

-- writeBlockToFile :: MemoryBlock a -> IO ()
-- writeBlockToFile mb = 





