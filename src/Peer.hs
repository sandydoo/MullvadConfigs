{-# LANGUAGE DeriveGeneric, OverloadedStrings, RecordWildCards, TemplateHaskell #-}
module Peer
  ( Peer(..)
  , fromFile
  ) where


import Data.Aeson
import Data.Aeson.Lens
import Data.Aeson.TH
import qualified Data.ByteString as BS
import Data.IP
import Data.Text
import GHC.Generics

import Data.CustomIP



data Peer =
  Peer
    { name       :: Text
    , publicKey  :: Text
    , privateKey :: Text
    , ipv4Addr   :: IPv4
    , ipv6Addr   :: IPv6
    , ports      :: [PortNumber]
    } deriving (Generic, Show)


$(deriveToJSON defaultOptions ''Peer)


instance FromJSON Peer where
  parseJSON =
    withObject "Peer" $ \o ->
      do  name       <- o .: "name"
          key        <- o .: "key"
          publicKey  <- key .: "public"
          privateKey <- key .: "private"
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
