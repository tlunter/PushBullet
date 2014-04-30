{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.PushBullet.Types (
    Device(..),
    Connection(..),
    PushBullet(..)
) where

import Control.Applicative
import Control.Monad.Reader
import Network.HTTP.Client
import Control.Monad.Catch

data Device = Device { iden :: String, name :: String }
    deriving (Show, Eq)

data Connection = Connection { apiKey :: String
                             , manager :: Manager
                             }

newtype PushBullet a = PushBullet (ReaderT Connection IO a)
    deriving (Monad, MonadIO, MonadThrow, MonadCatch, MonadReader Connection, Functor, Applicative)

