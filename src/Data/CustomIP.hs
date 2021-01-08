{-# LANGUAGE GeneralizedNewtypeDeriving, DerivingStrategies, TemplateHaskell #-}
module Data.CustomIP
  ( PortNumber
  ) where

import Data.Aeson
import Data.Aeson.TH
import Data.IP
import Data.Word



$(deriveFromJSON defaultOptions ''IPv4)
$(deriveToJSON   defaultOptions ''IPv4)

$(deriveFromJSON defaultOptions ''IPv6)
$(deriveToJSON   defaultOptions ''IPv6)


newtype PortNumber =
  PortNumber Word16
  deriving (Eq, Ord, Num, Enum, Bounded, Real, Integral, Show)
  deriving newtype (FromJSON, ToJSON)
