{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module AppEventEmit
  ( -- emitAppEvent,
    AppEventEmit (..),
  )
where

import Control.Algebra (Has, send)
import GHC.Base (Type)
import Control.Effect.TH

data AppEventEmit (m :: Type -> Type) k where
  EmitAppEvent :: String -> AppEventEmit m ()

-- emitAppEvent :: Has AppEventEmit sig m => String -> m ()
-- emitAppEvent = send . EmitAppEvent

makeSmartConstructors ''AppEventEmit
