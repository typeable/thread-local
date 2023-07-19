module Control.Concurrent.ThreadLocal ( ThreadLocal -- exported opaque
                                      , newThreadLocal
                                      , newThreadLocalWithGC
                                      , insertThreadLocal
                                      , fetchThreadLocal
                                      , deleteThreadLocal
                                      ) where

import Control.Concurrent.ThreadLocal.Internal
import Control.Concurrent.ThreadLocal.GC

-- | The version of 'newThreadLocal' that also launches a separate GC thread.
--   Takes the interval for GC in seconds. GC thread dies automatically once
--   'ThreadLocal' is collected.
newThreadLocalWithGC :: Word -> IO (ThreadLocal a)
newThreadLocalWithGC interval = do tls <- newThreadLocal
                                   addGC tls interval
