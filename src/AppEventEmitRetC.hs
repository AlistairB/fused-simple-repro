{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
module AppEventEmitRetC
  ( runAppEventEmitRet
  )
where

import Control.Algebra (Algebra(..), (:+:)(..), handleCoercible, Effect)
import Control.Effect.Writer (tell)
import Control.Carrier.Writer.Strict (runWriter, WriterC)
import AppEventEmit (AppEventEmit(..))

newtype AppEventEmitRetC m a = AppEventEmitRetC {runAppEventEmitRetC :: WriterC [String] m a}
  deriving (Functor, Applicative, Monad)

instance (Algebra sig m, Effect sig) => Algebra (AppEventEmit :+: sig) (AppEventEmitRetC m) where
  alg (L (EmitAppEvent appEvent k)) = AppEventEmitRetC (tell [appEvent]) *> k
  alg (R other) = AppEventEmitRetC (alg (handleCoercible (R other)))

runAppEventEmitRet :: AppEventEmitRetC m a -> m ([String], a)
runAppEventEmitRet = runWriter . runAppEventEmitRetC
