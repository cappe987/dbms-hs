module Decoder where


import Data.ByteString
import Data.ByteString.Char8 as C8
import Data.Serialize
import Data.Int
import Data.Either

import Control.Monad.Trans.State as ST

import Schema


-- What about the Either from Serialize.decode? Ignore it for now?

-- decodeInt :: ByteString -> Maybe (Int32, ByteString)
decodeInt :: State ByteString Int32
decodeInt = do
  bs <- ST.get
  let int = decode $ C8.take 4 bs
      res = fromRight 0 int :: Int32
  ST.put $ C8.drop 4 bs

  return res



decodeAppendChar :: String -> State ByteString String
decodeAppendChar s = do
  bs <- ST.get
  let res = decode bs :: Either String Char
      c = fromRight '\NUL' res
  ST.put $ Data.ByteString.drop 1 bs
  return $ c:s

decodeVarcharLoop :: Int -> String -> State ByteString String
decodeVarcharLoop 0   s = return $ Prelude.reverse s
decodeVarcharLoop len s = 
  decodeAppendChar s >>= decodeVarcharLoop (len-1)

decodeVarchar :: Int -> State ByteString String
decodeVarchar n = decodeVarcharLoop n ""



getDecoder :: SchemaType -> State ByteString RowValue
getDecoder SInt32       = RInt32  <$> decodeInt
getDecoder (SVarchar n) = RString <$> decodeVarchar n



decodeRow :: Schema -> ByteString -> Row
decodeRow ss bs = 
  let decoders = getDecoder <$> ss
      rowdecoder = sequenceA decoders :: State ByteString Row
  in evalState rowdecoder bs


