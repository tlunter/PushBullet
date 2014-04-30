module Web.PushBullet.Util (
    convertStringToByteString,
    convertByteStringToString
) where

import qualified Data.ByteString as B

convertEnum :: (Enum a, Enum b) => a -> b
convertEnum = toEnum . fromEnum

convertStringToByteString :: String -> B.ByteString
convertStringToByteString s = B.pack $ map convertEnum s

convertByteStringToString :: B.ByteString -> String
convertByteStringToString s = map convertEnum $ B.unpack s
