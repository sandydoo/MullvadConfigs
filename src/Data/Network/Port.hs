module Data.Network.Port
  ( Port(..)
  ) where


import Data.Aeson



newtype Port =
  Port Word16
  deriving newtype ( Eq, Ord, Show, FromJSON, ToJSON )
