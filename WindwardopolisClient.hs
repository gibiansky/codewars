{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module WindwardopolisClient (runClient) where

import ClassyPrelude
import Network.Simple.TCP hiding (send, sendTo, recv, recvFrom)
import Network.Socket hiding (connect, send, recv)
import Network.Socket.ByteString.Lazy (send, recv)
import Data.Binary.Put
import Data.Binary.Get
import qualified Data.ByteString.Lazy.Char8 as C

runClient :: HostName -> ServiceName -> ((String -> IO (), IO String) -> IO ()) -> IO ()
runClient host port prog =
  connect host port $ \(sock, sockAddr) -> prog (sendMessage sock, getMessage sock)

sendMessage :: Socket -> String -> IO ()
sendMessage sock msg
  -- The server expects exactly 4 bytes for the message length.
  | length msg > 2^32-1 = error "Message too long to send"
  | otherwise = send' len >> send' (C.pack msg)
      where len     = runPut $ putWord32le $ fromIntegral $ length msg
            send' m = void $ send sock m -- TODO: check length

getMessage :: Socket -> IO String
getMessage sock = do
  len <- runGet getWord32le <$> recv sock 4
  msg <- recv' sock $ fromIntegral len
  return $ C.unpack $ msg

recv' :: Socket -> Int64 -> IO C.ByteString
recv' sock len = recv'' sock "" len
  where recv'' sock bs remainingLen
          | remainingLen < 0  = error "I've seen too much!"
          | remainingLen == 0 = return bs
          | otherwise         = do
              more <- recv sock remainingLen
              recv'' sock (bs ++ more) (remainingLen - fromIntegral (length more))
