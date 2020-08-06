#!/usr/bin/env stack
{- stack script
    --ghc-options "-Wall"
    --resolver lts-16.8
    --package aeson,bytestring,containers,directory,http-conduit,lens,lens-aeson,text,utf8-string,zip
-}


{-# LANGUAGE DeriveGeneric, NamedFieldPuns, OverloadedStrings, RecordWildCards, TemplateHaskell #-}
import Codec.Archive.Zip as Zip
import Control.Lens
import Control.Monad (mzero)
import Data.Aeson
import Data.Aeson.Lens
import Data.Aeson.TH
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.Map as Map
import Data.Maybe (isJust, fromMaybe)
import qualified Data.Set as Set
import Data.Text as Text
import GHC.Generics
import qualified Network.HTTP.Simple as HTTP
import Prelude hiding (writeFile, unlines)
import qualified System.Directory as FS



serverListURL :: String
serverListURL = "https://api.mullvad.net/www/relays/all/"


preferredCountryCodes :: Set.Set Text
preferredCountryCodes = Set.fromList ["ch", "de", "gb", "nl", "se"]


type Emoji = Text

countryEmojis :: Map.Map Text Emoji
countryEmojis =
  Map.fromList
    [ ( "ch", "🇨🇭" )
    , ( "de", "🇩🇪" )
    , ( "gb", "🇬🇧" )
    , ( "nl", "🇳🇱" )
    , ( "se", "🇸🇪" )
    ]


data ServerInfo =
  ServerInfo
    { hostname :: Text
    , countryCode :: Text
    , countryName :: Text
    , cityCode :: Text
    , cityName :: Text
    , active :: Bool
    , owned :: Bool
    , provider :: Text
    , ipv4AddrIn :: Text
    , ipv6AddrIn :: Text
    , serverType :: Text
    , publicKey :: Text
    , multihopPort :: Int
    , socksName :: Text
    } deriving (Generic, Show)


$(deriveToJSON defaultOptions ''ServerInfo)


instance FromJSON ServerInfo where
  parseJSON (Object v) = ServerInfo
    <$> v .: "hostname"
    <*> v .: "country_code"
    <*> v .: "country_name"
    <*> v .: "city_code"
    <*> v .: "city_name"
    <*> v .: "active"
    <*> v .: "owned"
    <*> v .: "provider"
    <*> v .: "ipv4_addr_in"
    <*> v .: "ipv6_addr_in"
    <*> v .: "type"
    <*> v .: "pubkey"
    <*> v .: "multihop_port"
    <*> v .: "socks_name"

  parseJSON _ = mzero


data PeerInfo =
  PeerInfo
    { peerName :: Text
    , peerPublicKey :: Text
    , peerPrivateKey :: Text
    , peerIpv4Addr :: Text
    , peerIpv6Addr :: Text
    , peerPorts :: [Int]
    } deriving (Generic, Show)


$(deriveToJSON defaultOptions ''PeerInfo)


instance FromJSON PeerInfo where
  parseJSON (Object o) =
    do  peerName       <- o .: "name"
        peerKey        <- o .: "key"
        peerPublicKey  <- peerKey .: "public"
        peerPrivateKey <- peerKey .: "private"
        peerIpv4Addr   <- o .: "ipv4_address"
        peerIpv6Addr   <- o .: "ipv6_address"
        peerPorts      <- o .: "ports"

        return PeerInfo{..}

  parseJSON _ = mzero



-- Lens helpers


getCountryCode :: AsValue s => s -> Maybe Text
getCountryCode server = server ^? key "country_code" . _String


isPreferredCountryCode :: Text -> Maybe ()
isPreferredCountryCode code = preferredCountryCodes ^? ix code



-- Generate config


config :: PeerInfo -> ServerInfo -> Text
config PeerInfo{..} ServerInfo{..} =
  unlines $
    [ "[Interface]"
    , "PrivateKey = " <> peerPrivateKey
    , "Address = " <> peerIpv4Addr <> "," <> peerIpv6Addr
    , "DNS = 193.138.218.74"
    , "\n"
    , "[Peer]"
    , "PublicKey = " <> publicKey
    , "AllowedIPs = 0.0.0.0/0,::0/0"
    , "Endpoint = " <> ipv4AddrIn <> ":51820"
    ]



-- Helpers


filterServerList :: AsValue s => s -> [ServerInfo]
filterServerList serverList =
  serverList
  ^.. values
  . filteredBy (key "type"   . _String . only "wireguard")
  . filteredBy (key "active" . _Bool   . only True)
  . filtered (\server -> isJust $ getCountryCode server >>= isPreferredCountryCode)
  . _JSON


createName :: ServerInfo -> Text
createName ServerInfo{ hostname, cityName, countryCode, owned } =
  let serverCode = Text.takeWhile (/= '-') hostname

      countryEmoji = fromMaybe "" $ Map.lookup countryCode countryEmojis

      nameList = Text.intercalate "-" [countryEmoji, Text.toLower cityName, serverCode]

      newName = if owned then nameList <> "-🌟" else nameList
  in
    newName


createConfig :: PeerInfo -> ServerInfo -> FilePath -> IO ()
createConfig peer server configPath =
  do  let newConfig = config peer server
      let filePath = configPath <> "/" <> unpack (createName server) <> ".conf"

      BS.writeFile filePath (UTF8.fromString . unpack $ newConfig)


readPeerInfo :: IO [PeerInfo]
readPeerInfo =
  do  bytestring <- BS.readFile(".peers.json")

      case (decodeStrict bytestring) of
        Just peers ->
          return peers

        _ ->
          return []



main :: IO ()
main =
  do  request <- HTTP.parseRequest serverListURL
      serverList <- return . filterServerList . HTTP.getResponseBody =<< HTTP.httpBS request

      -- Load peers
      peers <- readPeerInfo

      currentPath <- FS.getCurrentDirectory

      mapM_
        (\peer ->
          do  let configPath = currentPath <> "/mullvad-" <> unpack (peerName peer)
              FS.createDirectoryIfMissing False configPath

              mapM_ (\server -> createConfig peer server configPath) serverList

              Zip.createArchive (configPath <> ".zip") (Zip.packDirRecur Zip.Deflate Zip.mkEntrySelector configPath)
        ) peers

      -- FS.removeDirectoryRecursive configPath

      print ("Configurations created." :: String)
