module Web.PushBullet.CoreSpec (spec) where

import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Web.PushBullet
import Test.Hspec

spec :: Spec
spec = do
    describe "getting devices with a bad API key" $ do
        it "returns an empty list" $ do
            withManager tlsManagerSettings $ \m -> do
                let a = ""
                    c = Connection { apiKey = a, manager = m }
                o <- runPushBullet c $ getDevices
                o `shouldBe` []
    describe "getting devices with a good API key" $ do
        it "returns an empty list" $ do
            withManager tlsManagerSettings $ \m -> do
                let a = ""
                    c = Connection { apiKey = a, manager = m }
                o <- runPushBullet c $ getDevices
                (length o > 0) `shouldBe` True
