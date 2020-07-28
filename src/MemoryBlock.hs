module MemoryBlock where 

import Data.ByteString
import Data.ByteString.Char8 as C8
import Data.Serialize
import Data.Int
import Control.Monad.Trans.State as ST
import Data.Either
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


-- type MemoryBlock = ByteString

-- newtype VarcharString = VarcharString String deriving Show

type MemoryBlock a = [a]
-- data MemoryBlock a = MemoryBlock [a]
-- newtype MemoryBlock a = MemoryBlock {
--     blockdata :: [a]
--   } deriving Show

-- -- newblock :: MemoryBlock a 
-- -- newblock = MemoryBlock {blockdata=[]}

-- instance Semigroup (MemoryBlock a) where
--   a <> b = MemoryBlock {blockdata=blockdata a <> blockdata b}

-- instance Monoid (MemoryBlock a) where
--   mempty = MemoryBlock {blockdata=[]}

-- -- instance Functor MemoryBlock where
-- --   fmap f a = MemoryBlock {blockdata = f <$> blockdata a}

-- -- data Block a = None | Some a
-- pure x = MemoryBlock {blockdata = [x]}

data DBTypes =
    DBInt 
  | DBString
  deriving Show

-- schema = [DBInt, DBString]

-- decodeInt :: ByteString -> Maybe (Int32, ByteString)
decodeInt :: State ByteString Int32
decodeInt = do
  bs <- ST.get
  let int = decode $ C8.take 4 bs
      res = fromRight 0 int :: Int32
  ST.put $ C8.drop 4 bs

  return res

  -- let int = decode $ C8.take 4 bs
  -- in case int of 
  --   Right a -> (a, C8.drop 4 bs)
  --   Left  _ -> undefined
-- decodeString :: State ByteString String
-- decodeString = do
--   bs <- ST.get
--   let str = decode bs :: Either String String
--       res = fromRight "" str 
--   ST.put $ Data.ByteString.drop (Prelude.length res) bs
--   return res
  -- let str = decode bs :: Either String String
  -- in case str of 
  --   Right s -> (s, Data.ByteString.drop 4 bs)
  --   Left  _ -> undefined

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

data Schema = 
  None
  | Some [String] [Int]

-- instance Serialize Schema where
--   put (Some ss is) = put ss <> put is
--   get (Some ss is) = 

encodeMB :: Serialize a => MemoryBlock a -> ByteString
encodeMB = Prelude.foldl (\acc s-> acc <> encode s) Data.ByteString.empty

-- data MemoryBlock a where
--   Block :: Serialize a => a -> MemoryBlock a
encodeString :: String -> ByteString
encodeString = C8.pack

encodeInt :: Int32 -> ByteString
encodeInt = encode

test :: Int32
test = 2147483647


testdecode :: MemoryBlock String -> Int
testdecode s = 
  case decode $ encodeMB s of
    (Right a) -> a
    (Left  b) -> undefined

-- unwrap (Block i) = i

blocksize :: Int
blocksize = 4096






