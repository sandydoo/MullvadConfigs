#!/usr/bin/env stack
-- stack script --ghc-options "-Wall" --resolver lts-15.5 --package aeson,containers,directory,http-conduit,lens,lens-aeson,text,zip


{-# LANGUAGE DeriveGeneric, NamedFieldPuns, OverloadedStrings, TemplateHaskell #-}
import Codec.Archive.Zip as Zip
import Control.Lens
import Control.Monad (mzero)
import Data.Aeson
import Data.Aeson.Lens
import Data.Aeson.TH
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Maybe (isJust)
import qualified Data.Set as Set
import Data.Text as Text
import GHC.Generics
import qualified Network.HTTP.Simple as HTTP
import qualified System.Directory as FS



serverListURL :: String
serverListURL = "https://api.mullvad.net/www/relays/all/"


validCountryCodes :: Set.Set Text
validCountryCodes = Set.fromList ["ch", "de", "gb", "nl", "se"]


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

  parseJSON _ = mzero



-- Lens helpers


getCountryCode :: AsValue s => s -> Maybe Text
getCountryCode server = server ^? key "country_code" . _String


isValidCountryCode :: Text -> Maybe ()
isValidCountryCode code = validCountryCodes ^? ix code



-- Helpers


filterServerList :: AsValue s => s -> [ServerInfo]
filterServerList serverList =
  serverList
  ^.. values
  . filteredBy (key "type"   . _String . only "wireguard")
  . filteredBy (key "active" . _Bool   . only True)
  . filtered (\server -> isJust $ getCountryCode server >>= isValidCountryCode)
  . _JSON


convertFilteredList :: ServerInfo -> (Text, Text)
convertFilteredList ServerInfo{ hostname, cityName, countryCode, owned } =
  case Map.lookup countryCode countryEmojis of
    Just countryEmoji ->
      let
        -- se1-malmo => se1
        serverCode = Text.takeWhile (/= '-') hostname
        nameList = Text.intercalate "-" [countryEmoji, Text.toLower cityName, serverCode]
        newName = if owned then nameList <> "-🌟" else nameList
      in
        (serverCode, newName)

    -- TODO: Redo
    Nothing -> ("", "")


createNewConfigs :: String -> String -> (Text, Text) -> IO ()
createNewConfigs currentPath configPath (serverCode, newName) =
  let
    oldPath = currentPath <> "/" <> ("mullvad-" <> unpack serverCode) <> ".conf"
    newPath = configPath  <> "/" <> unpack newName                    <> ".conf"
  in
    FS.copyFile oldPath newPath



main :: IO ()
main =
  do  request <- HTTP.parseRequest serverListURL
      serverList <- HTTP.httpBS request >>= return . HTTP.getResponseBody

      let filteredList = filterServerList serverList

      currentPath <- FS.getCurrentDirectory

      let renamingList = List.map convertFilteredList filteredList

      let configPath = currentPath <> "/MullvadConfigs"
      FS.createDirectoryIfMissing False configPath

      -- Copy valid configs
      mapM_ (createNewConfigs currentPath configPath) renamingList

      Zip.createArchive (configPath <> ".zip") (Zip.packDirRecur Zip.Deflate Zip.mkEntrySelector configPath)

      -- FS.removeDirectoryRecursive configPath

      print ("Configurations created." :: String)
