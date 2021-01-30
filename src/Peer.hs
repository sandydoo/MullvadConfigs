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
    { peerName       :: Text
    , peerPublicKey  :: Text
    , peerPrivateKey :: Text
    , peerIpv4Addr   :: IPRange
    , peerIpv6Addr   :: IPRange
    , peerPorts      :: [PortNumber]
    } deriving (Generic, Show)


$(deriveToJSON defaultOptions ''Peer)


instance FromJSON Peer where
  parseJSON =
    withObject "Peer" $ \o ->
      do  peerName       <- o .: "name"
          peerPublicKey  <- o .: "public_key"
          peerPrivateKey <- o .: "private_key"
          peerIpv4Addr   <- o .: "ipv4_address"
          peerIpv6Addr   <- o .: "ipv6_address"
          peerPorts      <- o .: "ports"

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
