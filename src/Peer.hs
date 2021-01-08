{-# LANGUAGE DeriveGeneric, OverloadedStrings, RecordWildCards, TemplateHaskell #-}
module Peer
  ( PeerInfo(..)
  , readPeerInfo
  ) where


import Data.Aeson
import Data.Aeson.Lens
import Data.Aeson.TH
import qualified Data.ByteString as BS
import Data.Text
import GHC.Generics



data PeerInfo =
  PeerInfo
    { peerName       :: Text
    , peerPublicKey  :: Text
    , peerPrivateKey :: Text
    , peerIpv4Addr   :: Text
    , peerIpv6Addr   :: Text
    , peerPorts      :: [Int]
    } deriving (Generic, Show)


$(deriveToJSON defaultOptions ''PeerInfo)


instance FromJSON PeerInfo where
  parseJSON =
    withObject "PeerInfo" $ \o ->
      do  peerName       <- o .: "name"
          peerKey        <- o .: "key"
          peerPublicKey  <- peerKey .: "public"
          peerPrivateKey <- peerKey .: "private"
          peerIpv4Addr   <- o .: "ipv4_address"
          peerIpv6Addr   <- o .: "ipv6_address"
          peerPorts      <- o .: "ports"

          return PeerInfo{..}



readPeerInfo :: IO [PeerInfo]
readPeerInfo =
  BS.readFile(".peers.json") >>=
    \bytestring ->
      case (decodeStrict bytestring) of
        Just peers ->
          return peers

        _ ->
          return []
