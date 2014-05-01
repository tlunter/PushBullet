{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.PushBullet.Types (
    Device(..),
    Response(..),
    defaultResponse,
    Connection(..),
    PushBullet(..)
) where

import Control.Applicative
import Control.Monad.Reader
import Network.HTTP.Client (Manager)
import Control.Monad.Catch

data Device = Device { deviceIden :: String, deviceName :: String }
    deriving (Show, Eq)

data Response = Response { responseIden :: String, responseCreated :: Int }
    deriving (Show, Eq)

defaultResponse :: Response
defaultResponse = Response { responseIden = "", responseCreated = -1 }

data Connection = Connection { apiKey :: String
                             , manager :: Manager
                             }

newtype PushBullet a = PushBullet (ReaderT Connection IO a)
    deriving (Monad, MonadIO, MonadThrow, MonadCatch, MonadReader Connection, Functor, Applicative)

