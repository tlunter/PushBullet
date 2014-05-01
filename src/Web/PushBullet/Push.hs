{-# LANGUAGE OverloadedStrings #-}
module Web.PushBullet.Push (
    pushNoteToAll,
    pushNoteToDevice
) where

import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString as B
import Text.JSON
import Control.Monad.Reader
import Network.HTTP.Client hiding (Response)
import Control.Monad.Catch

import Web.PushBullet.Types
import Web.PushBullet.Util

pushNoteToAll :: String -> String -> PushBullet Response
pushNoteToAll title body = do
    let params = [ ("type", "note")
                 , ("title", convertStringToByteString title)
                 , ("body", convertStringToByteString body)]
    push params
    `catch` \e -> do
        liftIO $ print (e :: HttpException)
        return defaultResponse

pushNoteToDevice :: String -> String -> Device -> PushBullet Response
pushNoteToDevice title body device = do
    let params = [ ("device_iden", convertStringToByteString $ deviceIden device)
                 , ("type", "note")
                 , ("title", convertStringToByteString title)
                 , ("body", convertStringToByteString body)]
    push params
    `catch` \e -> do
        liftIO $ print (e :: HttpException)
        return defaultResponse

push :: [(B.ByteString, B.ByteString)] -> PushBullet Response
push params = do
    conn <- ask
    let m = manager conn
        a = apiKey conn
    req <- liftIO $ parseUrl "https://api.pushbullet.com/api/pushes"
    let ar = applyBasicAuth (convertStringToByteString a) "" req
        pr = urlEncodedBody params ar
    res <- liftIO $ httpLbs pr m
    let b    = responseBody res
        json = decode (convertByteStringToString $ LB.toStrict b) :: Result (JSObject JSValue)
    return (readResponse json)

readResponse :: Result (JSObject JSValue) -> Response
readResponse (Ok x) = Response { responseIden = iden, responseCreated = created }
    where iden    = readString "iden" x
          created = readInt "created" x
readResponse _      = defaultResponse

readString :: String -> JSObject JSValue -> String
readString key obj = string $ jsString obj
    where jsString x = valFromObj key x :: Result JSString
          string (Ok x) = fromJSString x
          string _      = ""

readInt :: String -> JSObject JSValue -> Int
readInt key obj = int $ intRes $ jsString obj
    where jsString x = valFromObj key x :: Result JSValue
          intRes :: Result JSValue -> Result Int
          intRes (Ok x) = readJSON x :: Result Int
          intRes _      = Ok $ responseCreated defaultResponse
          int (Ok x)    = x
          int _         = responseCreated defaultResponse
