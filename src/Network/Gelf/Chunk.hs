{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
--
-- Module      :  Network.Gelf.Chunk
-- Copyright   :  Andy Georges
-- License     :  AllRightsReserved
--
-- Maintainer  :  itkovian@gmail.com
-- Stability   :  experimental
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Network.Gelf.Chunk (
    split
) where

import Data.Bits (shiftR, (.&.))
import qualified Data.ByteString.Lazy as BSL
import Data.Word


split :: Int              -- ^Chunk size
      -> Word64           -- ^Message identification (8 bytes)
      -> BSL.ByteString   -- ^Message to split into chunks
      -> [BSL.ByteString] -- ^Resulting chunks
split size id bs =
    let cs = split' bs
    in zipWith (chunkify (length cs)) cs [0..]
  where split' :: BSL.ByteString -> [BSL.ByteString]
        split' bs =
            if BSL.length bs == 0
                then []
                else let (h,ts) = BSL.splitAt (fromIntegral size) bs
                     in h : split' ts
        idBytes = BSL.pack [fromIntegral (shiftR id (x * 8) .&. 0xff) | x <- [7, 6 .. 0]]
        chunkify :: Int             -- ^Total number of chunks
                 -> BSL.ByteString  -- ^Chunk data
                 -> Int             -- ^Chunk number
                 -> BSL.ByteString  -- ^Resulting bytestring
        chunkify total s i = BSL.cons 0x1e $ BSL.cons 0x0f $ BSL.append idBytes $ BSL.cons (fromIntegral i :: Word8) $ BSL.cons (fromIntegral total :: Word8) s
