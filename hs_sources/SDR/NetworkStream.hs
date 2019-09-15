module SDR.NetworkStream (
        udpSendSocket,
        udpRecvSocket,
        udpSource,
        udpSink
    ) where

import Control.Monad
import Network.Socket (Socket, SockAddr)
import qualified Network.Socket as Socket
import Network.Socket.ByteString (sendAllTo, recv)
import Foreign.Storable
import qualified Data.Vector.Storable as VS
import Data.Vector.Storable.ByteString
import Pipes

udpSendSocket :: IO Socket
udpSendSocket = Socket.socket Socket.AF_INET Socket.Datagram Socket.defaultProtocol

udpRecvSocket
    :: SockAddr
    -> IO Socket
udpRecvSocket addr = do
    socket <- udpSendSocket
    Socket.bind socket addr
    return socket

udpSource 
    :: Storable a 
    => Socket 
    -> Int 
    -> Producer (VS.Vector a) IO ()
udpSource sock size = forever $ do
    s <- lift $ recv sock size
    yield $ byteStringToVector s

udpSink 
    :: Storable a 
    => Socket 
    -> SockAddr
    -> Consumer (VS.Vector a) IO ()
udpSink dev addr = for cat $ liftIO . flip (sendAllTo dev) addr . vectorToByteString
