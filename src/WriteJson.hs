{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module WriteJson
  ( writeJson,
    WriteJson (..),
  )
where

import Control.Algebra (Has, send)
import GHC.Base (Type)
import Control.Effect.TH
import Data.Aeson

data WriteJson (m :: Type -> Type) k where
  WriteJson :: ToJSON a => a -> WriteJson m ()

-- writeJson :: (Has WriteJson sig m, ToJSON a) => a -> m ()
-- writeJson = send . WriteJson

makeSmartConstructors ''WriteJson
