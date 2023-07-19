{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.ThreadLocal
import Control.Monad
import System.Random

main :: IO ()
main = do tls <- newThreadLocalWithGC 30
          replicateConcurrently_ 200000 (f tls)

f :: ThreadLocal Word -> IO ()
f tls = do v :: Word <- randomIO
           tid <- myThreadId
           insertThreadLocal v tls
           !v' <- fetchThreadLocal tls
           unless (Just v == v') $
             putStrLn ("Ooops! " <>  show tid <>  " expected " <>  show v <>  " while the fetched value is " <>  show v')
