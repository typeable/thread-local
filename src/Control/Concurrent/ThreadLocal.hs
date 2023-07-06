module Control.Concurrent.ThreadLocal ( ThreadLocal -- exported opaque
                                      , newThreadLocal
                                      , insertThreadLocal
                                      , fetchThreadLocal
                                      , removeThreadLocal
                                      ) where

import Control.Concurrent.ThreadLocal.Internal
