{-| Pipes utility functions -}
module SDR.PipeUtils (
    fork, 
    printStream,
    devnull,
    rate,
    ) where

import Data.Time.Clock
import Pipes
import qualified Pipes.Prelude as P
import Control.Monad

-- | Fork a pipe 
fork :: Monad m => Producer a m r -> Producer a (Producer a m) r
fork prod = runEffect $ hoist (lift . lift) prod >-> fork' 
    where 
    fork' = forever $ do
        res <- await
        lift $ yield res
        lift $ lift $ yield res

-- | A consumer that prints everything to stdout
printStream :: (Show a) => Int -> Consumer a IO ()
printStream samples = for cat $ lift . print 

-- | A consumer that discards everything
devnull :: Monad m => Consumer a m ()
devnull = forever await

-- | Passthrough pipe that prints the sample rate
rate :: Int -> Pipe a a IO b
rate samples = do
    start <- lift getCurrentTime 
    let rate' buffers = do
            res <- await

            time <- lift getCurrentTime 
            let diff = diffUTCTime time start 
                diffSecs :: Double
                diffSecs = fromRational $ toRational diff

            lift $ print $ buffers * fromIntegral samples / diffSecs

            yield res
            rate' (buffers + 1)
    rate' 1

