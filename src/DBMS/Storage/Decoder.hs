module DBMS.Storage.Decoder 
  ( decodeIntValue
  , decodeVarchar
  , decodeRow
  , decodeRows
  , decodeBlock 
  , getPointer
  , decodeInt)
  
  where


import Data.ByteString
import Data.ByteString.Char8 as C8
import Data.Serialize
import Data.Int
import Data.Either

import Control.Monad.Trans.State as ST

import DBMS.Schema.Types
import DBMS.Storage.Constants


-- What about the Either from Serialize.decode? Ignore it for now?


-- Type parsers

parseNullByte :: State ByteString Char
parseNullByte = do 
  bs <- ST.get
  let res = fromRight '\NUL' $ decode $ C8.take 1 bs :: Char
  ST.put $ C8.drop 1 bs
  return res
  
parsePointerInt :: State ByteString Int32
parsePointerInt = do
  bs <- ST.get
  let int = decode $ C8.take 4 bs
      res = fromRight 0 int :: Int32
  ST.put $ C8.drop 4 bs
  return res


parseMaybeInt :: State ByteString (Maybe Int32)
parseMaybeInt = do
  nullbyte <- parseNullByte

  if nullbyte == nullvalue then do
    bs <- ST.get
    ST.put $ C8.drop 4 bs
    return Nothing
  else 
    Just <$> parsePointerInt
    -- bs <- ST.get
    -- let int = decode $ C8.take 4 bs
    --     res = fromRight 0 int :: Int32
    -- ST.put $ C8.drop 4 bs

    -- return $ Just res

parseAppendChar :: String -> State ByteString String
parseAppendChar s = do
  bs <- ST.get
  let res = decode bs :: Either String Char
      c = fromRight '\NUL' res
  ST.put $ Data.ByteString.drop 1 bs
  return $ c:s

parseVarcharLoop :: Int32 -> String -> State ByteString String
parseVarcharLoop 0   s = return $ Prelude.reverse s
parseVarcharLoop len s = 
  parseAppendChar s >>= parseVarcharLoop (len-1)

parseMaybeVarchar :: Int32 -> State ByteString (Maybe String)
parseMaybeVarchar n = 
  do 
    nullbyte <- parseNullByte
    if nullbyte == nullvalue then do
      bs <- ST.get
      ST.put $ C8.drop (fromIntegral n) bs
      return Nothing -- Null found in string
    else 
      Just <$> parseVarcharLoop n ""



-- Get parser for matching schema
getParser :: ColumnSchema -> State ByteString ColValue
getParser ColumnSchema{schematype=SInt32    } = VInt32   <$> parseMaybeInt
getParser ColumnSchema{schematype=SVarchar n} = VString  <$> parseMaybeVarchar n





-- Parsing more advanced data

parseRow :: Schema -> State ByteString Row
parseRow ss = 
  let decoders = getParser <$> ss
      rowdecoder = sequenceA decoders :: State ByteString Row
  -- in evalState rowdecoder bs
  in rowdecoder



-- Removes padding for when the last row didn't quite take up all 
-- the bytes in the block, or if it's the last in the block.
removePaddingToPointer :: ByteString -> ByteString
removePaddingToPointer xs 
  | C8.length xs < 4 = undefined
  | otherwise = C8.reverse $ C8.take 4 $ C8.reverse xs

parsePointer :: State ByteString Int32
parsePointer = do
  bs <- ST.get
  let pointerBytes = removePaddingToPointer bs
  ST.put pointerBytes
  parsePointerInt



parseRows :: Schema -> Int32 -> State ByteString [Row]
parseRows schema 0 = return []
parseRows schema n = do 
  bs  <- ST.get
  row <- parseRow schema

  (:) <$> return row <*> parseRows schema (n-1)



-- Public decoders

decodeIntValue :: ByteString -> Maybe Int32
decodeIntValue = evalState parseMaybeInt

decodeVarchar :: Int32 -> ByteString -> Maybe String
decodeVarchar n = evalState (parseMaybeVarchar n)

getPointer :: ByteString -> Int32
getPointer = evalState parsePointer

decodeInt :: ByteString -> Int32
decodeInt = evalState parsePointerInt

decodeRow :: Schema -> ByteString -> Row
decodeRow schema = evalState (parseRow schema)

decodeRows :: Schema -> Int32 -> ByteString -> [Row]
decodeRows schema n = evalState (parseRows schema n)

decodeBlock :: Schema -> ByteString -> ([Row], Int32)
decodeBlock schema = 
  evalState ((,) <$> (parsePointerInt >>= parseRows schema) <*> parsePointer)