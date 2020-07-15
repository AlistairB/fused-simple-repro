{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
module EmitErrorsC (EmitErrorsC(..)) where

import Control.Algebra ((:+:) (..), Algebra (..), handleCoercible)
import Control.Carrier.Error.Either
import Control.Monad.IO.Class (MonadIO)
import GHC.Base (Type)
import AppEventEmit
import Control.Monad.Except (ExceptT(..))

newtype EmitErrorsC (e :: Type) m a = EmitErrorsC {runEmitErrorsC :: ErrorC e m a}
  deriving (Functor, Applicative, Monad, MonadIO)

hoistEither :: Applicative m => Either e a -> ExceptT e m a
hoistEither e = ExceptT (pure e)

instance (Effect sig, Algebra sig m, Has AppEventEmit sig m) => Algebra (Error e :+: sig) (EmitErrorsC e m) where
  alg (L (L (Throw e))) = do
    emitAppEvent "boom"
    EmitErrorsC $ ErrorC $ hoistEither (Left e)
  alg (L (R (Catch m h k))) =
    EmitErrorsC $
      ErrorC $
        ExceptT $
          runCatch m >>= \case
            Left err -> runCatch (h err >>= k)
            Right result -> runCatch (k result)

  alg (R other) = EmitErrorsC (alg (R (handleCoercible other)))

runCatch ::
     EmitErrorsC e m a
  -> m (Either e a)
runCatch =
  runError . runEmitErrorsC
