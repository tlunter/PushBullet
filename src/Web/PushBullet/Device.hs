{-# LANGUAGE OverloadedStrings #-}
module Web.PushBullet.Device (
    getDevices
) where

import qualified Data.ByteString.Lazy as LB
import Text.JSON
import Web.PushBullet.Types
import Control.Monad.Reader
import Network.HTTP.Client
import Control.Monad.Catch

import Web.PushBullet.Util

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

getListOfDevices :: Result (JSObject JSValue) -> Result [JSObject JSValue]
getListOfDevices (Ok x)  = valFromObj "devices" x
getListOfDevices _       = decode "[]"

readListOfDevices :: Result [JSObject JSValue] -> [Device]
readListOfDevices (Ok x) = map readDevice x
readListOfDevices _      = []

readDevice :: JSObject JSValue -> Device
readDevice x = Device { iden = deviceIden, name = deviceName }
    where deviceIden     = readIden $ getIden x
          deviceExtras   = getExtras x
          deviceNickname = readNickname deviceExtras
          deviceModel    = readModel deviceExtras
          deviceName     = if not (null deviceNickname)
                        then deviceNickname
                        else deviceModel

getIden :: JSObject JSValue -> Result JSString
getIden = valFromObj "iden"

readIden :: Result JSString -> String
readIden (Ok x) = fromJSString x
readIden _      = ""

getExtras :: JSObject JSValue -> Result (JSObject JSValue)
getExtras = valFromObj "extras"

readNickname :: Result (JSObject JSValue) -> String
readNickname (Ok object) = getNicknameString (readJSON nickname :: Result JSString)
    where nicknameStrings = filter (\(key, _) -> (key == "nickname")) $ fromJSObject object
          (_, nickname)   = head nicknameStrings
          getNicknameString (Ok val) = fromJSString val
          getNicknameString _        = ""
readNickname _           = ""

readModel :: Result (JSObject JSValue) -> String
readModel (Ok object) = getModelString (readJSON model :: Result JSString)
    where modelStrings = filter (\(key, _) -> (key == "model")) $ fromJSObject object
          (_, model)   = head modelStrings
          getModelString (Ok val) = fromJSString val
          getModelString _        = ""
readModel _           = ""

