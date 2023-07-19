module Control.Concurrent.ThreadLocal.Internal where

import Control.Concurrent (ThreadId, myThreadId)
import Control.Concurrent.Async
import Control.Concurrent.MVar
import Data.Map (Map)
import qualified Data.Map.Strict as Map

type ThreadMap a = Map ThreadId a

data ThreadLocal a = ThreadLocal { mvar :: MVar (ThreadMap a)
                                 , gcThread :: Maybe (Async ())
                                 }

-- | Create a new thread local storage of type 'a'
--   Note that TLS created by this function will accumulate dead threads
--   and thus leak memory during its lifetime. This is mostly fine for shorter-living
--   instances. If you plan on having this 'ThreadLocal' live longer than a while,
--   consider using 'newThreadLocalWithGC' instead.
newThreadLocal :: IO (ThreadLocal a)
newThreadLocal = do var <- newMVar Map.empty
                    return (ThreadLocal var Nothing)

-- | Insert a value for the current thread
insertThreadLocal :: a -> ThreadLocal a -> IO ()
insertThreadLocal val (ThreadLocal var _) = do tid <- myThreadId
                                               modifyMVar_ var (pure . Map.insert tid val)

-- | Fetch a value for the current thread. If none was inserted previously, return Nothing.
fetchThreadLocal :: ThreadLocal a -> IO (Maybe a)
fetchThreadLocal (ThreadLocal var _) = do tid <- myThreadId
                                          m <- readMVar var
                                          return (Map.lookup tid m)

-- | Delete a currently held value for this thread.
deleteThreadLocal :: ThreadLocal a -> IO ()
deleteThreadLocal (ThreadLocal var _) = do tid <- myThreadId
                                           modifyMVar_ var (pure . Map.delete tid)

