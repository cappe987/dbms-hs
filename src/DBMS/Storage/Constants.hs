module DBMS.Storage.Constants where

import Data.Int

-- Just to test with
intMax :: Int32
intMax = 2147483647



nullpointer :: Int32
nullpointer = 0

nullvalue :: Char
nullvalue = '\NUL'

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