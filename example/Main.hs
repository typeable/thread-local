{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Concurrent
import Control.Concurrent.ThreadLocal
import Control.Monad
import System.Random

main :: IO ()
main = do tls <- newThreadLocal
          replicateM_ 1000 (forkIO $ f tls)
          threadDelay 1000000

f :: ThreadLocal Word -> IO ()
f tls = do v :: Word <- randomIO
           tid <- myThreadId
           insertThreadLocal v tls
           v' <- fetchThreadLocal tls
           unless (Just v == v') $
             fail ("Ooops! " <> show tid <> " expected " <> show v <> " while the fetched value is " <> show v')
