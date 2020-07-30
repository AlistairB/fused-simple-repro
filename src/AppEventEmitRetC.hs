{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
module AppEventEmitRetC
  ( runAppEventEmitRet
  )
where

import Control.Algebra (Algebra(..), (:+:)(..))
import Control.Effect.Writer (tell)
import Control.Carrier.Writer.Strict (runWriter, WriterC)
import AppEventEmit (AppEventEmit(..))

newtype AppEventEmitRetC m a = AppEventEmitRetC {runAppEventEmitRetC :: WriterC [String] m a}
  deriving (Functor, Applicative, Monad)

instance Algebra sig m => Algebra (AppEventEmit :+: sig) (AppEventEmitRetC m) where
  alg hdl sig ctx = AppEventEmitRetC $ case sig of
    L (EmitAppEvent appEvent) -> ctx <$ tell [appEvent]
    R other     -> alg (runAppEventEmitRetC . hdl) (R other) ctx

runAppEventEmitRet :: AppEventEmitRetC m a -> m ([String], a)
runAppEventEmitRet = runWriter . runAppEventEmitRetC
