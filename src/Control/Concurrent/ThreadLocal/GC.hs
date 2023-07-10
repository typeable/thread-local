module Control.Concurrent.ThreadLocal.GC where

import Control.Concurrent.ThreadLocal.Internal

import Control.Concurrent.Async
import Control.Concurrent.MVar
import Data.Either
import qualified Data.Map as Map
import GHC.Conc
import System.Mem.Weak

-- | Check whether a thread is still alive.
isThreadAlive :: ThreadId -> IO Bool
isThreadAlive tid = isAlive <$> threadStatus tid
  where isAlive ThreadRunning = True
        isAlive (ThreadBlocked _) = True
        isAlive _ = False

-- | Sort alive from dead
splitAlive :: [ThreadId] -> IO ([ThreadId], [ThreadId])
splitAlive = fmap partitionEithers
           . traverse (\t -> s t <$> isThreadAlive t)
  where s x b = if b then Left x else Right x

collectGarbage :: ThreadMap a -> IO (ThreadMap a)
collectGarbage m = do (_live, dead) <- splitAlive (Map.keys m)
                      pure (foldr Map.delete m dead)

gcLoop :: Weak (MVar (ThreadMap a)) -> Word -> IO ()
gcLoop wvar t = do threadDelay (fromIntegral t * 1000000)
                   var' <- deRefWeak wvar
                   whenJust var' $ \var ->
                     do modifyMVar_ var collectGarbage
                        gcLoop wvar t

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust (Just v) f = f v
whenJust Nothing _ = pure ()
{-# INLINE whenJust #-}

addGC :: ThreadLocal a -> Word -> IO (ThreadLocal a)
addGC (ThreadLocal var _) interval =
  do wvar <- mkWeakMVar var (pure ())
     a <- async (gcLoop wvar interval)
     labelThread (asyncThreadId a) "ThreadLocal GC Loop"
     pure (ThreadLocal var (Just a))
