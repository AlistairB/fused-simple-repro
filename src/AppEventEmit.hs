{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module AppEventEmit
  ( emitAppEvent,
    AppEventEmit (..),
  )
where

import Control.Algebra (Has, send)
import GHC.Base (Type)
import Control.Effect.TH

data AppEventEmit (e :: Type) (m :: Type -> Type) k where
  EmitAppEvent :: e -> AppEventEmit e m ()

-- emitAppEvent :: Has (AppEventEmit e) sig m => e -> m ()
-- emitAppEvent = send . EmitAppEvent

makeSmartConstructors ''AppEventEmit
