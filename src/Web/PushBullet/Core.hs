{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.PushBullet.Core (
    Device(..),
    Connection(..),
    runPushBullet,
    getDevices
) where

import Control.Applicative
import Control.Monad.Reader
import Network.HTTP.Client
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import Web.PushBullet.Util
import Text.JSON
import Control.Monad.Catch

data Device = Device { iden :: String }
    deriving (Show, Eq)

data Connection = Connection { apiKey :: String
                             , manager :: Manager
                             }

newtype PushBullet a = PushBullet (ReaderT Connection IO a)
    deriving (Monad, MonadIO, MonadThrow, MonadCatch, MonadReader Connection, Functor, Applicative)

runPushBullet :: Connection -> PushBullet a -> IO a
runPushBullet conn (PushBullet pb) = runReaderT pb conn

getDevices :: PushBullet [Device]
getDevices = do
    conn <- ask
    let m = manager conn
        a = apiKey conn
    req <- liftIO $ parseUrl "https://api.pushbullet.com/api/devices"
    let r = applyBasicAuth (convertStringToByteString a) "" req
    res <- liftIO $ httpLbs r m
    let b    = responseBody res
        json = decode (convertByteStringToString $ LB.toStrict b) :: Result (JSObject JSValue)
    return (readListOfDevices $ getListOfDevices json)
    `catch` \e -> do
        liftIO $ print (e :: HttpException)
        return []
    where getListOfDevices :: Result (JSObject JSValue) -> Result [JSObject JSValue]
          getListOfDevices (Ok x)  = valFromObj "devices" x
          getListOfDevices _       = decode "[]"
          readListOfDevices :: Result [JSObject JSValue] -> [Device]
          readListOfDevices (Ok x) = map readDevice x
          readListOfDevices _      = []
          readDevice :: JSObject JSValue -> Device
          readDevice x = Device { iden = readIden $ getIden x }
          getIden :: JSObject JSValue -> Result JSString
          getIden x = valFromObj "iden" x
          readIden (Ok x) = fromJSString x
          readIden _      = ""
