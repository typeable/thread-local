module Control.Concurrent.ThreadLocal.Internal where

import Control.Concurrent (ThreadId, myThreadId)
import Data.IORef
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import System.Mem.StableName
import System.Mem.Weak

type ThreadName = StableName ThreadId

data LocalVal a = LocalVal !(Weak ThreadId) !a

isSameThread :: ThreadId -> Weak ThreadId -> IO Bool
isSameThread tid wid = do mtid' <- deRefWeak wid
                          return $ case mtid' of
                            Nothing -> False
                            Just tid' -> tid == tid'

newtype ThreadLocal a = ThreadLocal { values :: IORef (Map ThreadName (LocalVal a))
                                    }

-- | Create a new thread local storage of type 'a'
newThreadLocal :: IO (ThreadLocal a)
newThreadLocal = do ref <- newIORef Map.empty
                    return (ThreadLocal ref)

-- | Insert a value for the current thread
insertThreadLocal :: a -> ThreadLocal a -> IO ()
insertThreadLocal v (ThreadLocal ref) = do tid <- myThreadId
                                           name <- makeStableName tid
                                           wid <- mkWeakPtr tid Nothing
                                           atomicModifyIORef' ref $ \m ->
                                             let m' = Map.insert name (LocalVal wid v) m
                                             in  (m',())

-- | Fetch a value for the current thread. If none was inserted previously, return Nothing.
fetchThreadLocal :: ThreadLocal a -> IO (Maybe a)
fetchThreadLocal (ThreadLocal ref) = do tid <- myThreadId
                                        name <- makeStableName tid
                                        m <- readIORef ref
                                        case Map.lookup name m of
                                          Just (LocalVal wid v) ->
                                            do st <- isSameThread tid wid
                                               if st
                                                 then return (Just v)
                                                 -- Account for the fact that a string representation of
                                                 -- a ThreadId can result in a collision.
                                                 -- If one occurs, remove the old data.
                                                 else Nothing <$ removeThreadLocal (ThreadLocal ref)
                                          Nothing -> return Nothing

-- | Remove the currently held value for the current thread.
removeThreadLocal :: ThreadLocal a -> IO ()
removeThreadLocal (ThreadLocal ref) = do tid <- myThreadId
                                         name <- makeStableName tid
                                         atomicModifyIORef' ref $ \m ->
                                           (Map.delete name m, ())
