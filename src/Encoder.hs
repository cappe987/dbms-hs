module Encoder where

import Prelude as P
import Data.ByteString
import Data.ByteString.Char8 as C8
import Data.Serialize
import Data.Int

import Schema

encodeString :: String -> ByteString
encodeString = C8.pack

encodeVarchar :: Int -> String -> ByteString
encodeVarchar n s = encodeString (fill $ P.reverse s)
  where len = P.length
        fill s
          | len s < n = fill ('\NUL':s)
          | len s > n = P.reverse $ P.drop (len s - n) s
          | otherwise = P.reverse s
    



encodeInt :: Int32 -> ByteString
encodeInt = encode


encodeRowValue :: SchemaType -> RowValue -> ByteString
encodeRowValue SInt32       (RInt32  d) = encodeInt d
encodeRowValue (SVarchar n) (RString s) = encodeVarchar n s
encodeRowValue _ _ = undefined --fail "Handle this earlier in the process"

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

