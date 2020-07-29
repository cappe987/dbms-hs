module Encoder where

import Data.ByteString
import Data.ByteString.Char8 as C8
import Data.Serialize
import Data.Int

import Schema

encodeString :: String -> ByteString
encodeString = C8.pack

encodeVarchar :: Int -> String -> ByteString
encodeVarchar n s = encodeString (fill $ Prelude.reverse s)
  where len = Prelude.length
        fill s
          | len s < n = fill ('\NUL':s)
          | len s > n = Prelude.reverse $ Prelude.drop (len s - n) s
          | otherwise = Prelude.reverse s
    



encodeInt :: Int32 -> ByteString
encodeInt = encode


encodeRowValue :: SchemaType -> RowValue -> Maybe ByteString
encodeRowValue SInt32       (RInt32  d) = Just $ encodeInt d
encodeRowValue (SVarchar n) (RString s) = Just $ encodeVarchar n s
encodeRowValue _ _ = Nothing --fail "Handle this earlier in the process"

-- Maybe add error messages here, for when the schema doesn't match input.
-- But that should probably be checked earlier.
encodeRow :: Schema -> Row -> Maybe ByteString
encodeRow [] [] = Just Data.ByteString.empty
encodeRow (s:xs) (r:rs) = 
  let bs = encodeRowValue s r
  in (<>) <$> bs <*> encodeRow xs rs
encodeRow _ _ = Nothing

