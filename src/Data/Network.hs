{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Network
  ( PortNumber
  , module Data.IP
  ) where

import Data.Aeson
import Data.Aeson.TH
import Data.IP
import Data.Text
import Text.Read (readMaybe)
import Data.Word



newtype PortNumber =
  PortNumber Word16
  deriving (Eq, Ord, Num, Enum, Bounded, Real, Integral, Show)


instance FromJSON PortNumber where
  parseJSON w = parseJSON w >>= \w -> return $ PortNumber w

instance ToJSON PortNumber where
  toJSON (PortNumber port) = toJSON port



-- JSON instances for IP addresses


instance FromJSON IPRange where
  parseJSON (String w) = parseFromText "Couln't parse IPRange" w

instance ToJSON IPRange where
  toJSON = toString


instance FromJSON IPv4 where
  parseJSON (String w) = parseFromText "Couln't parse IPv4" w

instance ToJSON IPv4 where
  toJSON = toString


instance FromJSON IPv6 where
  parseJSON (String w) = parseFromText "Couln't parse IPv6" w

instance ToJSON IPv6 where
  toJSON = toString



-- Helpers


parseFromText :: (Read a, MonadFail m) => String -> Text -> m a
parseFromText error w =
  case readMaybe (unpack w) of
    Nothing ->
      fail error

    Just v  ->
      return v


toString :: Show a => a -> Value
toString = String . pack . show
