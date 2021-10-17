{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Network.Port
  ( Port(..)
  ) where


import Data.Aeson



newtype Port =
  Port Word16
  deriving ( Eq, Ord, Num, Enum, Bounded, Real, Integral, Show )


instance FromJSON Port where
  parseJSON w =
    do  word16 <- parseJSON w
        return $ Port word16


instance ToJSON Port where
  toJSON ( Port port ) = toJSON port
