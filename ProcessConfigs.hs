#!/usr/bin/env stack
{- stack script
    --ghc-options "-Wall"
    --resolver lts-16.15
    --package aeson,bytestring,containers,directory,filepath,http-conduit,iproute,lens,lens-aeson,text,utf8-string,zip
-}


{-# LANGUAGE DeriveGeneric, NamedFieldPuns, OverloadedStrings, RecordWildCards, TemplateHaskell #-}
import Codec.Archive.Zip as Zip
import Control.Lens hiding ((<.>))
import Control.Monad (forM_)
import Data.Aeson
import Data.Aeson.Lens
import Data.Aeson.TH
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as UTF8
import Data.IP (IPRange)
import qualified Data.Map as Map
import           Data.Map (Map)
import Data.Maybe (isJust, fromMaybe)
import qualified Data.Set as Set
import           Data.Set (Set)
import Data.Text as Text
import GHC.Generics
import qualified Network.HTTP.Simple as HTTP
import Prelude hiding (writeFile, unlines)
import qualified System.Directory as FS
import System.FilePath ((</>), (<.>))



serverListURL :: String
serverListURL = "https://api.mullvad.net/www/relays/all/"


preferredCountryCodes :: Set Text
preferredCountryCodes = Set.fromList [ "ch", "de", "gb", "nl", "se" ]


type Emoji = Text

countryEmojis :: Map Text Emoji
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
  parseJSON =
    withObject "ServerInfo" $ \o ->
      ServerInfo
        <$> o .: "hostname"
        <*> o .: "country_code"
        <*> o .: "country_name"
        <*> o .: "city_code"
        <*> o .: "city_name"
        <*> o .: "active"
        <*> o .: "owned"
        <*> o .: "provider"
        <*> o .: "ipv4_addr_in"
        <*> o .: "ipv6_addr_in"
        <*> o .: "type"
        <*> o .: "pubkey"
        <*> o .: "multihop_port"
        <*> o .: "socks_name"


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



-- Lens helpers


getCountryCode :: AsValue s => s -> Maybe Text
getCountryCode server = server ^? key "country_code" . _String


isPreferredCountryCode :: Text -> Maybe ()
isPreferredCountryCode code = preferredCountryCodes ^? ix code



-- Generate config


publicIPRanges :: Set IPRange
publicIPRanges = Set.fromList [ "1.0.0.0/8", "2.0.0.0/8", "3.0.0.0/8", "4.0.0.0/6", "8.0.0.0/7", "11.0.0.0/8", "12.0.0.0/6", "16.0.0.0/4", "32.0.0.0/3", "64.0.0.0/2", "128.0.0.0/3", "160.0.0.0/5", "168.0.0.0/6", "172.0.0.0/12", "172.32.0.0/11", "172.64.0.0/10", "172.128.0.0/9", "173.0.0.0/8", "174.0.0.0/7", "176.0.0.0/4", "192.0.0.0/9", "192.128.0.0/11", "192.160.0.0/13", "192.169.0.0/16", "192.170.0.0/15", "192.172.0.0/14", "192.176.0.0/12", "192.192.0.0/10", "193.0.0.0/8", "194.0.0.0/7", "196.0.0.0/6", "200.0.0.0/5", "208.0.0.0/4", "193.138.218.74/32", "::/0" ]


mullvadIPRange :: IPRange
mullvadIPRange = "10.64.0.0/10"


createConfig :: PeerInfo -> ServerInfo -> Text
createConfig PeerInfo{..} ServerInfo{..} =
  let
    allowedIPs = Set.insert mullvadIPRange publicIPRanges

    serializeIPs :: Set IPRange -> Text
    serializeIPs ips =
      Set.foldr packIP "" ips

    packIP :: IPRange -> Text -> Text
    packIP ip ips
      | Text.null ips = fromIPRange ip <> ips
      | otherwise     = fromIPRange ip <> ", " <> ips

    fromIPRange :: IPRange -> Text
    fromIPRange = pack . show
  in
  unlines $
    [ "[Interface]"
    , "PrivateKey = " <> peerPrivateKey
    , "Address = " <> peerIpv4Addr <> "," <> peerIpv6Addr
    , "DNS = 193.138.218.74"
    , "\n"
    , "[Peer]"
    , "PublicKey = " <> publicKey
    -- Explicitly send traffic for public IP ranges through the tunnel, excluding private / LAN ranges.
    -- To send instead everything through tunnel: 0.0.0.0/0,::0/0
    , "AllowedIPs = " <> serializeIPs allowedIPs
    , "Endpoint = " <> ipv4AddrIn <> ":51820"
    ]



-- Helpers


filterServerList :: AsValue s => s -> [ServerInfo]
filterServerList serverList =
  serverList
  ^.. values
  . filteredBy (key "type"   . _String . only "wireguard")
  . filteredBy (key "active" . _Bool   . only True)
  . filtered
      (\server ->
        isJust $
          getCountryCode server >>= isPreferredCountryCode
      )
  . _JSON


createName :: ServerInfo -> Text
createName ServerInfo{ hostname, cityName, countryCode, owned } =
  let
    serverCode =
      Text.takeWhile (/= '-') hostname

    countryEmoji =
      fromMaybe "" $
        Map.lookup countryCode countryEmojis

    nameList =
      Text.intercalate "-"
        [countryEmoji, Text.toLower cityName, serverCode]

    newName =
      if owned
      then nameList <> "-🌟"
      else nameList
  in
    newName


createConfigFile :: PeerInfo -> ServerInfo -> FilePath -> IO ()
createConfigFile peer server configPath =
  let
    newConfig =
      createConfig peer server

    toUTF8 =
      UTF8.fromString . unpack

    filePath =
      configPath </> unpack (createName server) <.> "conf"
  in
    BS.writeFile filePath (toUTF8 newConfig)


readPeerInfo :: IO [PeerInfo]
readPeerInfo =
  BS.readFile(".peers.json") >>=
    \bytestring ->
      case (decodeStrict bytestring) of
        Just peers ->
          return peers

        _ ->
          return []


-- TODO: add some proper error handling.
main :: IO ()
main =
  do  putStrLn "Fetching current server list..."

      request <- HTTP.parseRequest serverListURL
      serverList <- return . filterServerList . HTTP.getResponseBody =<< HTTP.httpBS request

      putStrLn "Reading local peer list..."
      peers <- readPeerInfo

      currentPath <- FS.getCurrentDirectory

      forM_ peers $ \peer ->
        do  let name = unpack $ peerName peer
            let configPath = currentPath </> "configs" </> name

            putStrLn $ "Creating configs for " <> name <> "..."
            FS.createDirectoryIfMissing True configPath

            forM_ serverList $ \server ->
              createConfigFile peer server configPath

            putStrLn $ "Creating zip archive for " <> name <> "..."
            Zip.createArchive (configPath <.> "zip") $
              Zip.packDirRecur Zip.Deflate Zip.mkEntrySelector configPath

      putStrLn "Configurations created."
