module Web.PushBullet.CoreSpec (spec) where

import Network.HTTP.Client hiding (Response)
import Network.HTTP.Client.TLS
import Web.PushBullet
import Test.Hspec

pushbulletApiKey :: String
pushbulletApiKey = ""

spec :: Spec
spec = do
    -- Getting devices
    describe "getting devices with a bad API key" $
        it "returns an empty list" $
            withManager tlsManagerSettings $ \m -> do
                let a = ""
                    c = Connection { apiKey = a, manager = m }
                o <- runPushBullet c getDevices
                null o `shouldBe` True
    describe "getting devices with a good API key" $
        it "returns a non empty list" $
            withManager tlsManagerSettings $ \m -> do
                let a = pushbulletApiKey
                    c = Connection { apiKey = a, manager = m }
                o <- runPushBullet c getDevices
                not (null o) `shouldBe` True

    -- Pushing notes
    describe "pushing notes with a bad API key" $
        it "returns an empty list" $
            withManager tlsManagerSettings $ \m -> do
                let a = ""
                    c = Connection { apiKey = a, manager = m }
                o <- runPushBullet c $ pushNoteToAll "Bad Test" "Bad"
                o `shouldBe` defaultResponse
    describe "pushing notes with a good API key" $
        it "returns a non empty list" $
            withManager tlsManagerSettings $ \m -> do
                let a = pushbulletApiKey
                    c = Connection { apiKey = a, manager = m }
                o <- runPushBullet c $ pushNoteToAll "Good Test" "Success!"
                responseCreated o > 0 `shouldBe` True
    describe "pushing notes to a device with a good API key" $
        it "returns a non empty list" $
            withManager tlsManagerSettings $ \m -> do
                let a = pushbulletApiKey
                    c = Connection { apiKey = a, manager = m }
                o <- runPushBullet c $ do
                    devices <- getDevices
                    pushNoteToDevice "Good Test" "Success!" $ head devices
                responseCreated o > 0 `shouldBe` True
