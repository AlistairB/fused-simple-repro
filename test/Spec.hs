{-# LANGUAGE TypeApplications #-}

import EmitErrorsC
import Test.Hspec
import Control.Carrier.Throw.Either
import Control.Carrier.Error.Either
import AppEventEmitRetC
import Control.Category ((>>>))
import Data.Functor.Identity

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $
  fcontext "When emitting errors" $ do
    it "no errors produces a right" $ do
      let handler =
                runEmitErrorsC @String
            >>> runError @String
            >>> runAppEventEmitRet
            >>> runIdentity

          input = liftEither (Right 1 :: Either String Int)
          (appEvents, eitherResult) = handler input

      appEvents `shouldBe` []
      eitherResult `shouldBe` Right 1

    it "an error produces a log + left" $ do
      let handler =
                runEmitErrorsC @String
            >>> runError @String
            >>> runAppEventEmitRet
            >>> runIdentity

          input = liftEither (Left "Woah" :: Either String Int)
          (appEvents, eitherResult) = handler input

      length appEvents `shouldBe` 1
      eitherResult `shouldBe` Left "Woah"
