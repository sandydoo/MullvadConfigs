{-# LANGUAGE DeriveGeneric, NamedFieldPuns, OverloadedStrings, TemplateHaskell #-}
module Server
  ( ServerInfo(..)
  , createName
  , filterServerList
  ) where


import Control.Lens
import Data.Aeson
import Data.Aeson.Lens
import Data.Aeson.TH
import qualified Data.Map as Map
import           Data.Map (Map)
import Data.Maybe (catMaybes, isJust)
import GHC.Generics
import qualified Data.Set as Set
import           Data.Set (Set)
import Data.Text as Text



preferredCountryCodes :: Set Text
preferredCountryCodes = Set.fromList [ "ch", "de", "gb", "nl", "se" ]



data ServerInfo =
  ServerInfo
    { hostname     :: Text
    , countryCode  :: Text
    , countryName  :: Text
    , cityCode     :: Text
    , cityName     :: Text
    , active       :: Bool
    , owned        :: Bool
    , provider     :: Text
    , ipv4AddrIn   :: Text
    , ipv6AddrIn   :: Text
    , serverType   :: Text
    , publicKey    :: Text
    , multihopPort :: Int
    , socksName    :: Text
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



filterServerList :: AsValue s => s -> [ServerInfo]
filterServerList serverList =
  serverList
  ^.. values
  . filteredBy (key "type"   . _String . only "wireguard")
  . filteredBy (key "active" . _Bool   . only True)
  . filtered (\server -> isJust $ isPreferredCountryCode =<< getCountryCode server)
  . _JSON


createName :: ServerInfo -> Text
createName ServerInfo{ hostname, cityName, countryCode, owned } =
  let
    countryEmoji =
      Map.lookup countryCode countryEmojis

    lowerCityName =
      Just $ Text.toLower cityName

    serverCode =
      Just $ Text.takeWhile (/= '-') hostname

    preferredServer =
      if owned then Just "🌟" else Nothing

    nameList =
      [ countryEmoji, lowerCityName, serverCode, preferredServer ]

  in
    Text.intercalate "-" . catMaybes $ nameList



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



-- Lens helpers


getCountryCode :: AsValue s => s -> Maybe Text
getCountryCode server = server ^? key "country_code" . _String


isPreferredCountryCode :: Text -> Maybe ()
isPreferredCountryCode code = preferredCountryCodes ^? ix code
