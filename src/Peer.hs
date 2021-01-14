{-# LANGUAGE DeriveGeneric, TemplateHaskell #-}
module Peer
  ( Peer(..)
  , fromFile
  ) where


import Data.Aeson
import Data.Aeson.TH
import qualified Data.ByteString as BS

import Data.Network



data Peer =
  Peer
    { name       :: Text
    , publicKey  :: Text
    , privateKey :: Text
    , ipv4Addr   :: IPRange
    , ipv6Addr   :: IPRange
    , ports      :: [PortNumber]
    } deriving (Generic, Show)


$(deriveToJSON defaultOptions ''Peer)


instance FromJSON Peer where
  parseJSON =
    withObject "Peer" $ \o ->
      do  name       <- o .: "name"
          publicKey  <- o .: "public_key"
          privateKey <- o .: "private_key"
          ipv4Addr   <- o .: "ipv4_address"
          ipv6Addr   <- o .: "ipv6_address"
          ports      <- o .: "ports"

          return Peer{..}



fromFile :: String -> IO [Peer]
fromFile filename =
  BS.readFile filename >>=
    \bytestring ->
      case (decodeStrict bytestring) of
        Just peers ->
          return peers

        _ ->
          return []
