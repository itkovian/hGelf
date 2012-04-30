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
    send
  , encode
) where

import Codec.Compression.GZip (compress)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BSL
import Data.Maybe (isJust)
import qualified Data.Text as T
import Network.BSD (getHostName)
import System.Time (getClockTime, ClockTime(TOD))

{- TODO:
 - timestamp: specs state microsecond timestamp, so correct this
 -
 -}

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
                    ++ (map (\(k, v) -> (k, A.toJSON `fmap` v)) fields)
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
    in if (BSL.length bs) + 2 < (fromIntegral chunkSize)
          then [BSL.cons 0x1f $ BSL.cons 0x8b bs]
          else undefined



-- | Send a log message to a server accepting Graylog2 messages.
send :: T.Text                      -- ^Short message
     -> Maybe T.Text                -- ^Long message (optional)
     -> Maybe T.Text                -- ^Filename of the message cause
     -> Maybe Integer               -- ^Line in the file where the message was sent for
     -> [(T.Text, Maybe T.Text)]    -- ^Additional fields (name, information), should not contain 'id' as name
     -> IO ()                       -- ^Does I/O
send shortMessage longMessage filename lineNumber fields = do
    hostname <- getHostName
    timestamp <- getClockTime >>= (\(TOD seconds _) -> return seconds)
    let ms = encode 256 shortMessage longMessage hostname timestamp filename lineNumber fields
    return ()
