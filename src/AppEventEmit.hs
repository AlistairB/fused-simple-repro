{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
module AppEventEmit
  ( emitAppEvent,
    AppEventEmit (..),
  )
where

import Control.Algebra (Has, send)
import Control.Effect.Class (Effect, HFunctor)
import GHC.Generics (Generic1)

data AppEventEmit m k
  = EmitAppEvent String (m k)
  deriving (Functor, Generic1)

instance HFunctor AppEventEmit
instance Effect AppEventEmit

emitAppEvent :: Has AppEventEmit sig m => String -> m ()
emitAppEvent = send . flip EmitAppEvent (pure ())
