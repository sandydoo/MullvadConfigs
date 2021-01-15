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
    { pName       :: Text
    , pPublicKey  :: Text
    , pPrivateKey :: Text
    , pIpv4Addr   :: IPRange
    , pIpv6Addr   :: IPRange
    , pPorts      :: [PortNumber]
    } deriving (Generic, Show)


$(deriveToJSON defaultOptions ''Peer)


instance FromJSON Peer where
  parseJSON =
    withObject "Peer" $ \o ->
      do  pName       <- o .: "name"
          pPublicKey  <- o .: "public_key"
          pPrivateKey <- o .: "private_key"
          pIpv4Addr   <- o .: "ipv4_address"
          pIpv6Addr   <- o .: "ipv6_address"
          pPorts      <- o .: "ports"

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
