{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module WindwardopolisClient (runClient) where

import ClassyPrelude
import Network.Simple.TCP hiding (send, sendTo, recv, recvFrom)
import Network.Socket hiding (connect, send, recv)
import Network.Socket.ByteString.Lazy (send, recv)
import Data.Binary.Put
import Data.Binary.Get
import qualified Data.ByteString.Lazy.Char8 as C

import Types
import XML

runClient :: HostName -> ServiceName -> ((Command -> IO (), IO Message) -> IO ()) -> IO ()
runClient host port prog =
  connect host port $ \(sock, sockAddr) -> prog (sendMessage sock, getMessage sock)

debug :: Bool
debug = True

logger :: String -> IO ()
logger str =
  when debug $ putStrLn $ pack str

sendMessage :: Socket -> Command -> IO ()
sendMessage sock cmd
  -- The server expects exactly 4 bytes for the message length.
  | length msg > 2^32-1 = error "Message too long to send"
  | otherwise = do
      logger msg
      send' len 
      send' (C.pack msg)
  where msg = XML.encodeCommand cmd
        len     = runPut $ putWord32le $ fromIntegral $ length msg
        send' m = void $ send sock m -- TODO: check length

getMessage :: Socket -> IO Message
getMessage sock = do
  len <- runGet getWord32le <$> recv sock 4
  msg <- recv' sock $ fromIntegral len
  let str = C.unpack msg
  logger str
  return $ parseMessage str

recv' :: Socket -> Int64 -> IO C.ByteString
recv' sock = recv'' sock ""
  where recv'' sock bs remainingLen
          | remainingLen < 0  = error "I've seen too much!"
          | remainingLen == 0 = return bs
          | otherwise         = do
              more <- recv sock remainingLen
              recv'' sock (bs ++ more) (remainingLen - fromIntegral (length more))
