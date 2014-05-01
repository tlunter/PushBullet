{-# LANGUAGE OverloadedStrings #-}
module Web.PushBullet.Device (
    getDevices
) where

import qualified Data.ByteString.Lazy as LB
import Text.JSON
import Control.Monad.Reader
import Network.HTTP.Client
import Control.Monad.Catch

import Web.PushBullet.Types
import Web.PushBullet.Util

getDevices :: PushBullet [Device]
getDevices = do
    conn <- ask
    let m = manager conn
        a = apiKey conn
    req <- parseUrl "https://api.pushbullet.com/api/devices"
    let r = applyBasicAuth (convertStringToByteString a) "" req
    res <- liftIO $ httpLbs r m
    let b    = responseBody res
        json = decode (convertByteStringToString $ LB.toStrict b) :: Result (JSObject JSValue)
    return (readListOfDevices json)
    `catch` \e -> do
        liftIO $ print (e :: HttpException)
        return []

readListOfDevices :: Result (JSObject JSValue) -> [Device]
readListOfDevices (Ok x) = deviceMapper $ getListOfDevices x
    where getListOfDevices :: JSObject JSValue -> Result [JSObject JSValue]
          getListOfDevices = valFromObj "devices"
          deviceMapper :: Result [JSObject JSValue] -> [Device]
          deviceMapper (Ok jsonDevice) = map readDevice jsonDevice
          deviceMapper _               = []
readListOfDevices _      = []

readDevice :: JSObject JSValue -> Device
readDevice x = Device { deviceIden = iden, deviceName = name }
    where iden     = readIden $ getIden x
          extras   = readExtras x
          nickname = readString "nickname" extras
          model    = readString "model" extras
          name     = if not (null nickname)
                        then nickname
                        else model

getIden :: JSObject JSValue -> Result JSString
getIden = valFromObj "iden"

readIden :: Result JSString -> String
readIden (Ok x) = fromJSString x
readIden _      = ""

readExtras :: JSObject JSValue -> JSObject JSValue
readExtras obj = objFromObj $ valFromObj "extras" obj
    where objFromObj :: Result (JSObject JSValue) -> JSObject JSValue
          objFromObj (Ok x) = x
          objFromObj _      = toJSObject []

readString :: String -> JSObject JSValue -> String
readString key obj = string $ jsString obj
    where jsString x = valFromObj key x :: Result JSString
          string (Ok x) = fromJSString x
          string _      = ""
