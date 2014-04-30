module Web.PushBullet.Core (
    Device(..),
    Connection(..),
    runPushBullet,
    getDevices
) where

import Control.Monad.Reader
import Web.PushBullet.Types
import Web.PushBullet.Device

runPushBullet :: Connection -> PushBullet a -> IO a
runPushBullet conn (PushBullet pb) = runReaderT pb conn
