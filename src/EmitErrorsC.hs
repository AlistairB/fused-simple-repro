{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
module EmitErrorsC (EmitErrorsC(..)) where

import Control.Algebra ((:+:) (..), Algebra (..))
import Control.Carrier.Error.Either
import Control.Monad.IO.Class (MonadIO)
import GHC.Base (Type)
import AppEventEmit
import Control.Monad.Except (ExceptT(..))
import Control.Monad.Trans.Except (catchE)

newtype EmitErrorsC (e :: Type) m a = EmitErrorsC {runEmitErrorsC :: ErrorC e m a}
  deriving (Functor, Applicative, Monad, MonadIO)

hoistEither :: Applicative m => Either e a -> ExceptT e m a
hoistEither e = ExceptT (pure e)

instance (Algebra sig m, Has AppEventEmit sig m) => Algebra (Error e :+: sig) (EmitErrorsC e m) where
  alg hdl sig ctx = case sig of
    (L (L (Throw e))) -> do
      emitAppEvent "boom"
      EmitErrorsC $ ErrorC $ hoistEither (Left e)
    (L (R (Catch m h))) ->
      EmitErrorsC $
        ErrorC $
          catchE (runCatch $ hdl (m <$ ctx)) (runCatch . hdl . (<$ ctx) . h)
    (R other) -> alg hdl (R other) ctx

runCatch ::
     EmitErrorsC e m a
  -> ExceptT e m a
runCatch =
  ExceptT . runError . runEmitErrorsC
