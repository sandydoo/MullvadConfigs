{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Network
  ( PortNumber
  , module Data.IP
  ) where

import Data.Aeson
import Data.IP
import Data.Text



newtype PortNumber =
  PortNumber Word16
  deriving (Eq, Ord, Num, Enum, Bounded, Real, Integral, Show)


instance FromJSON PortNumber where
  parseJSON w =
    do  word16 <- parseJSON w
        return (PortNumber word16)

instance ToJSON PortNumber where
  toJSON (PortNumber port) = toJSON port



-- JSON instances for IP addresses


instance FromJSON IPRange where
  parseJSON (String w) = parseFromText "Couln't parse IPRange" w

instance ToJSON IPRange where
  toJSON = toJSONString

instance ToText IPRange where
  toText = pack . show


instance FromJSON IPv4 where
  parseJSON (String w) = parseFromText "Couln't parse IPv4" w

instance ToJSON IPv4 where
  toJSON = toJSONString

instance ToText IPv4 where
  toText = pack . show


instance FromJSON IPv6 where
  parseJSON (String w) = parseFromText "Couln't parse IPv6" w

instance ToJSON IPv6 where
  toJSON = toJSONString

instance ToText IPv6 where
  toText = pack . show



-- Helpers


parseFromText :: (Read a, MonadFail m) => String -> Text -> m a
parseFromText err w =
  case readMaybe (unpack w) of
    Nothing ->
      fail err

    Just v  ->
      return v


toJSONString :: Show a => a -> Value
toJSONString = String . pack . show
