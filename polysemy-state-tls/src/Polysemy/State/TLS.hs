{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module Polysemy.State.TLS where

import Control.Concurrent.ThreadLocal
import Data.Maybe (fromMaybe)
import Polysemy
import Polysemy.State


-- | Evaluate the 'State' effect using 'ThreadLocal' semantincs.
--   Note that this can only work over state being wrapped in 'Maybe', since the storage for the current thread
--   can be empty. For an alternative, see 'runTLStateWithDefault'.
runTLState :: forall s r a. Member (Embed IO) r => ThreadLocal s -> Sem (State (Maybe s) ': r) a -> Sem r a
runTLState tls = interpret f
  where f :: forall r' x. State (Maybe s) (Sem r') x -> Sem r x
        f Get = embed (fetchThreadLocal tls)
        f (Put (Just v)) = embed (insertThreadLocal v tls)
        f (Put Nothing) = embed (deleteThreadLocal tls)

-- | Evaluate the 'State' effect using 'ThreadLocal' semantincs.
--   The default value needs to be provided for cases where the value for the given thread isn't yet set,
--   i.e. 'get' is called before 'put'.
runTLStateWithDefault :: forall s r a. Member (Embed IO) r => s -> ThreadLocal s -> Sem (State s ': r) a -> Sem r a
runTLStateWithDefault def tls = interpret f
  where f :: forall r' x. State s (Sem r') x -> Sem r x
        f Get = fromMaybe def <$> embed (fetchThreadLocal tls)
        f (Put v) = embed (insertThreadLocal v tls)
