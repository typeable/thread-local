module Control.Concurrent.ThreadLocal ( ThreadLocal -- exported opaque
                                      , newThreadLocal
                                      , insertThreadLocal
                                      , fetchThreadLocal
                                      , gcThreadLocal
                                      , dumpThreadLocal
                                      , printThreadLocal
                                      ) where

import Control.Concurrent.ThreadLocal.Internal
