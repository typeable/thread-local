{-# LANGUAGE TupleSections #-}
module Control.Concurrent.ThreadLocal.Internal where

import Control.Concurrent (ThreadId, myThreadId, mkWeakThreadId)
import Control.Concurrent.MVar
import Control.Monad ((<=<), guard)
import Data.Foldable (find, toList)
import Data.Maybe (isJust)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Witherable
import System.Mem.Weak

data LocalValue a = LocalValue {-# UNPACK #-} !(Weak ThreadId)  -- ^ ThreadId reference used as a key
                                              String            -- ^ String representation of ThreadId, for debugging
                                              a

localThread :: LocalValue a -> IO (Maybe ThreadId)
localThread (LocalValue weak _ _) = deRefWeak weak

derefLocal :: LocalValue a -> IO (Maybe (ThreadId, a))
derefLocal (LocalValue weak _ a) = fmap (,a) <$> deRefWeak weak

makeLocal :: ThreadId -> a -> IO (LocalValue a)
makeLocal tid a = do weak <- mkWeakThreadId tid
                     pure (LocalValue weak (show tid) a)

newtype ThreadLocal a = ThreadLocal { values :: MVar (Vector (LocalValue a))
                                    }

-- | Create a new thread local storage of type 'a'
newThreadLocal :: IO (ThreadLocal a)
newThreadLocal = do var <- newMVar Vector.empty
                    return (ThreadLocal var)

-- | Insert a value for the current thread
insertThreadLocal :: a -> ThreadLocal a -> IO ()
insertThreadLocal val (ThreadLocal var) = do tid <- myThreadId
                                             loc <- makeLocal tid val
                                             modifyMVar_ var $ \v ->
                                               do ts <- filterA (\l -> do tid' <- localThread l
                                                                          pure (isJust tid' && Just tid /= tid')
                                                                ) (toList v)
                                                  pure (Vector.fromList (loc:ts))

-- | Fetch a value for the current thread. If none was inserted previously, return Nothing.
fetchThreadLocal :: ThreadLocal a -> IO (Maybe a)
fetchThreadLocal (ThreadLocal var) = do tid <- myThreadId
                                        v <- readMVar var
                                        f tid <$> wither derefLocal v
  where f tid vec = do (_,v) <- find ((== tid) . fst) vec
                       pure v

gcThreadLocal :: ThreadLocal a -> IO ()
gcThreadLocal (ThreadLocal var) = modifyMVar_ var (filterA (fmap isJust . localThread))

dumpThreadLocal :: ThreadLocal a -> IO [(Maybe ThreadId, String, a)]
dumpThreadLocal (ThreadLocal var) = traverse (\l@(LocalValue _ s v) -> (,s,v) <$> localThread l) . toList =<< readMVar var

printThreadLocal :: Show a => ThreadLocal a -> IO ()
printThreadLocal = print <=< dumpThreadLocal
