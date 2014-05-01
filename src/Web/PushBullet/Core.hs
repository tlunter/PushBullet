module Web.PushBullet.Core (
    runPushBullet
) where

import Control.Monad.Reader
import Web.PushBullet.Types

runPushBullet :: Connection -> PushBullet a -> IO a
runPushBullet conn (PushBullet pb) = runReaderT pb conn
