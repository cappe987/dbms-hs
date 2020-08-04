module DBMS.Storage.Encoder where

import Prelude as P
import Data.ByteString
import Data.ByteString.Char8 as C8
import Data.Serialize
import Data.Int

import Debug.Trace

import DBMS.Schema.Types
import DBMS.Schema.Information
import DBMS.Storage.Constants

-- Documentation says C8.pack can be a bottleneck
encodeString :: String -> ByteString
encodeString = C8.pack

encodeVarchar :: Int32 -> String -> ByteString
encodeVarchar n s = encodeString (fill $ P.reverse s)
  where len  s = fromIntegral $ P.length s
        fill s
          | len s < n = fill ('\NUL':s)
          | len s > n = P.reverse $ P.drop (fromIntegral $ len s - n) s
          | otherwise = P.reverse s
    



encodeInt :: Int32 -> ByteString
encodeInt = encode


encodeRowValue :: Column -> ColValue -> ByteString
encodeRowValue Column{typeof=SInt32    } (RInt32 d) = encodeInt d
encodeRowValue Column{typeof=SVarchar n} (RString s) = encodeVarchar n s
encodeRowValue a b = trace (show a ++ show b) undefined--fail "Handle this earlier in the process"

-- Maybe add error messages here, for when the schema doesn't match input.
-- But that should probably be checked earlier.
encodeRow :: Schema -> Row -> ByteString
-- encodeRow [] [] = Data.ByteString.empty
-- encodeRow (s:xs) (r:rs) = 
--   let bs = encodeRowValue s r
--   in bs <> encodeRow xs rs
-- encodeRow _ _ = undefined

encodeRow ss rs = 
  if P.length ss == P.length rs then
    P.foldl (<>) Data.ByteString.empty $ P.zipWith encodeRowValue ss rs
  else
    undefined


encodeRows :: Schema -> [Row] -> ByteString
encodeRows schema = 
  P.foldl (\acc r-> acc <> encodeRow schema r) Data.ByteString.empty





padBytestring :: Int -> ByteString
padBytestring n = 
  P.foldl (<>) Data.ByteString.empty (P.replicate n (encode '\NUL'))

padMemoryBlock :: Int -> Int -> ByteString
padMemoryBlock rowsize rowcount = 
  padBytestring (blocksize - (blockmetadata + rowsize*rowcount + pointersize)) 



-- Creates block without the 4 pointer bytes at the end
-- Ignores any rows that don't fit
-- Maybe make it return a list of the rows that didn't fit?
encodeBlockNoPointer :: TableDetails -> [Row] -> ByteString
encodeBlockNoPointer details rows = 
  let size           = fromIntegral $ rowsize details
      len            = P.length rows
      maxAmount      = actualsize `div` size 
      nrRowsToEncode = min maxAmount len
      rowsInBlock    = encodeInt (fromIntegral nrRowsToEncode)
      bs             = encodeRows (schema details) (P.take nrRowsToEncode rows)
  in rowsInBlock <> bs <> padMemoryBlock size nrRowsToEncode

-- Ignores any rows that don't fit
encodeBlock :: TableDetails -> [Row] -> Int32 -> ByteString
encodeBlock details rows pointer = 
  encodeBlockNoPointer details rows <> encodeInt pointer




-- Note: add handling for schemas that are larger 
-- than (blocksize - pointersize - blockmetadata) bytes.
-- Returns a list of ByteStrings where the last 4 bytes for pointersize are not added.
-- So the calling function can fill in the pointers or null.
-- This might be useful when rehashing.
createBlocks :: Schema -> [Row] -> [ByteString]
createBlocks schema [] = []
createBlocks schema xs = 
  let size           = fromIntegral $ getRowsize schema
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


  

