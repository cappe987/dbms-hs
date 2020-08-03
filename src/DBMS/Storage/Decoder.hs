module DBMS.Storage.Decoder 
  ( decodeInt
  , decodeVarchar
  , decodeRow
  , decodeRows
  , decodeBlock 
  , getPointer)
  
  where


import Data.ByteString
import Data.ByteString.Char8 as C8
import Data.Serialize
import Data.Int
import Data.Either

import Control.Monad.Trans.State as ST

import DBMS.Schema.Types


-- What about the Either from Serialize.decode? Ignore it for now?


-- Type parsers

parseInt :: State ByteString Int32
parseInt = do
  bs <- ST.get
  let int = decode $ C8.take 4 bs
      res = fromRight 0 int :: Int32
  ST.put $ C8.drop 4 bs

  return res

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

parseVarchar :: Int32 -> State ByteString String
parseVarchar n = parseVarcharLoop n ""



-- Get parser for matching schema
getParser :: Column -> State ByteString ColValue
getParser Column{typeof=SInt32    } = RInt32   <$> parseInt
getParser Column{typeof=SVarchar n} = RString  <$> parseVarchar n





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
  parseInt



parseRows :: Schema -> Int32 -> State ByteString [Row]
parseRows schema 0 = return []
parseRows schema n = do 
  bs  <- ST.get
  row <- parseRow schema

  (:) <$> return row <*> parseRows schema (n-1)



-- Public decoders

decodeInt :: ByteString -> Int32
decodeInt = evalState parseInt

decodeVarchar :: Int32 -> ByteString -> String
decodeVarchar n = evalState (parseVarchar n)

getPointer :: ByteString -> Int32
getPointer = evalState parsePointer

decodeRow :: Schema -> ByteString -> Row
decodeRow schema = evalState (parseRow schema)

decodeRows :: Schema -> Int32 -> ByteString -> [Row]
decodeRows schema n = evalState (parseRows schema n)

decodeBlock :: Schema -> ByteString -> ([Row], Int32)
decodeBlock schema = 
  evalState ((,) <$> (parseInt >>= parseRows schema) <*> parsePointer)