module Control.Concurrent.ThreadLocal ( ThreadLocal -- exported opaque
                                      , newThreadLocal
                                      , newThreadLocalWithGC
                                      , insertThreadLocal
                                      , fetchThreadLocal
                                      , deleteThreadLocal
                                      ) where

import Control.Concurrent.ThreadLocal.Internal
import Control.Concurrent.ThreadLocal.GC

newThreadLocalWithGC :: Word -> IO (ThreadLocal a)
newThreadLocalWithGC interval = do tls <- newThreadLocal
                                   addGC tls interval
