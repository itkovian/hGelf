{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
--
-- Module      :  Network.Gelf
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

module Network.Gelf (
    GelfConnection
  , mkGelfConnection
  , closeGelfConnection
  , Network.Gelf.send
  , encode
) where

import Codec.Compression.Zlib (compress)
import Control.Arrow (second)
import qualified Data.Aeson as A
import Data.Bits(shiftL, (.|.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import Data.Digest.Pure.MD5 (md5)
import Data.Maybe (isJust)
import qualified Data.Text as T
import qualified Data.Serialize as DS (encode)
import Data.Word
import Network.BSD (getHostName)
import Network.Socket
import qualified Network.Socket.ByteString.Lazy as NSBL
import System.Time (getClockTime, ClockTime(TOD))

import Network.Gelf.Chunk (split)

{- TODO:
 - * timestamp: specs state microsecond timestamp, so correct this
 - * wrap this in a monad stack with IO at the bottom, and some static
 -   configuration info on top of that, e.g., for setting the hostname,
 -   the destination port, chunk size, etc.
 -}

data GelfConnection = GelfConnection
    { graylogHost   :: String
    , graylogPort   :: Int
    , graylogChunk  :: Int
    , graylogSocket :: Socket
    }

mkGelfConnection :: String          -- ^Host name of the Graylog server
                 -> Int             -- ^Port number
                 -> Int             -- ^Chunk size
                 -> IO GelfConnection  -- ^Resulting connection record
mkGelfConnection serverName serverPort chunkSize = do
    addressInfos <- getAddrInfo Nothing (Just serverName) (Just $ show serverPort)
    let serverAddress = head addressInfos -- FIXME: this should handle errors too
    sock <- socket (addrFamily serverAddress) Datagram defaultProtocol
    return $ GelfConnection { graylogHost = serverName
                            , graylogPort = serverPort
                            , graylogChunk = chunkSize
                            , graylogSocket = sock
                            }

closeGelfConnection :: GelfConnection -- ^The connection to the Graylog server
                    -> IO ()
closeGelfConnection conn = sClose $ graylogSocket conn

catSecondMaybes :: [(a, Maybe b)]
                -> [(a, b)]
catSecondMaybes [] = []
catSecondMaybes ((k, v):vs) =
    case v of
        Just v' -> (k,v') : cvs
        Nothing -> cvs
  where cvs = catSecondMaybes vs


gelfMessage :: T.Text                   -- ^Short message
            -> Maybe T.Text             -- ^Long message (optional)
            -> String                   -- ^Hostname
            -> Integer                  -- ^Timestamp
            -> Maybe T.Text             -- ^Filename (optional)
            -> Maybe Integer            -- ^Line number (optional)
            -> [(T.Text, Maybe T.Text)] -- ^Additional fields
            -> A.Value
gelfMessage shortMessage longMessage hostname timestamp filename lineNumber fields =
    let loglevel = 1 :: Int
        allFields = [ ("version", A.toJSON `fmap` Just ("1.0" :: T.Text))
                    , ("host" , A.toJSON `fmap` Just hostname)
                    , ("short_message", A.toJSON `fmap` Just shortMessage)
                    , ("full_message", A.toJSON `fmap` longMessage)
                    , ("timestamp", A.toJSON `fmap` Just timestamp)
                    , ("level", A.toJSON `fmap` Just loglevel)
                    , ("facility", A.toJSON `fmap` Just ("GELF" :: T.Text))
                    , ("line", A.toJSON `fmap` Just lineNumber)
                    , ("file", A.toJSON `fmap` Just filename) ]
                    ++ map (second (fmap A.toJSON)) fields
    in A.object $ catSecondMaybes allFields


-- | Encode a log message as a GELF message.
--
-- This function wraps a given log message in a GELF structure. It creates the
-- JSON object, converts it to a ByteString and GZips the result.
-- If the resulting ByteString is longer than the maximal chunk size,
-- the GELF message is split up into chunks, each at most chunk size in length.
encode :: Int                         -- ^Maximal chunk size
       -> T.Text                      -- ^Short message
       -> Maybe T.Text                -- ^Long message (optional)
       -> String                      -- ^Hostname
       -> Integer                     -- ^Timestamp
       -> Maybe T.Text                -- ^Filename of the file causing the message, e.g., for debugging purposes
       -> Maybe Integer               -- ^Line number in the file causing the message, e.g., for debugging purposes
       -> [(T.Text, Maybe T.Text)]    -- ^Additional fields
       -> [BSL.ByteString]            -- ^One or more chunks
encode chunkSize shortMessage longMessage hostname timestamp filename lineNumber fields =
    let j = gelfMessage shortMessage longMessage hostname timestamp filename lineNumber fields
        bs = compress $ A.encode j
    in if BSL.length bs + 2 < fromIntegral chunkSize
          then [bs]
          else split chunkSize id bs
  where id = foldl1 (\w b -> shiftL w 8 .|. b) . map fromIntegral . BS.unpack . BS.take 8 . DS.encode . md5 . BSLC.pack $ hostname ++ show timestamp :: Word64


-- | Send a log message to a server accepting Graylog2 messages.
send :: GelfConnection              -- ^Connection to the Graylog server
     -> T.Text                      -- ^Short message
     -> Maybe T.Text                -- ^Long message (optional)
     -> Maybe T.Text                -- ^Filename of the message cause
     -> Maybe Integer               -- ^Line in the file where the message was sent for
     -> [(T.Text, Maybe T.Text)]    -- ^Additional fields (name, information), should not contain 'id' as name
     -> IO ()                       -- ^Does I/O
send connection shortMessage longMessage filename lineNumber fields = do
    hostname <- getHostName
    timestamp <- getClockTime >>= (\(TOD seconds _) -> return seconds)
    let sock = graylogSocket connection
        chunkSize = graylogChunk connection
        ms = encode chunkSize shortMessage longMessage hostname timestamp filename lineNumber fields
    mapM_ (NSBL.send sock) ms
