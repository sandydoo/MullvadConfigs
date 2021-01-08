{-# LANGUAGE DeriveGeneric, NamedFieldPuns, OverloadedStrings, TemplateHaskell #-}
module Server
  ( Server(..)
  , createName
  , filterServerList
  ) where


import Control.Lens
import Data.Aeson
import Data.Aeson.Lens
import Data.Aeson.TH
import Data.IP
import qualified Data.Map as Map
import           Data.Map (Map)
import Data.Maybe (catMaybes, isJust)
import GHC.Generics
import qualified Data.Set as Set
import           Data.Set (Set)
import Data.Text as Text

import Data.CustomIP
import Data.CountryFlag as CountryFlag



preferredCountryCodes :: Set Text
preferredCountryCodes = Set.fromList [ "ch", "de", "gb", "nl", "se" ]



data Server =
  Server
    { hostname     :: Text
    , countryCode  :: Text
    , countryName  :: Text
    , cityCode     :: Text
    , cityName     :: Text
    , active       :: Bool
    , owned        :: Bool
    , provider     :: Text
    , ipv4AddrIn   :: IPv4
    , ipv6AddrIn   :: IPv6
    , serverType   :: Text
    , publicKey    :: Text
    , multihopPort :: PortNumber
    , socksName    :: Text
    } deriving (Generic, Show)


$(deriveToJSON defaultOptions ''Server)


instance FromJSON Server where
  parseJSON =
    withObject "Server" $ \o ->
      Server
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



filterServerList :: AsValue s => s -> [Server]
filterServerList serverList =
  serverList
  ^.. values
  . filteredBy (key "type"   . _String . only "wireguard")
  . filteredBy (key "active" . _Bool   . only True)
  . filtered (\server -> isJust $ isPreferredCountryCode =<< getCountryCode server)
  . _JSON


createName :: Server -> Text
createName Server{ hostname, cityName, countryCode, owned } =
  let
    countryEmoji =
      CountryFlag.fromCountryCode countryCode

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



-- Lens helpers


getCountryCode :: AsValue s => s -> Maybe Text
getCountryCode server = server ^? key "country_code" . _String


isPreferredCountryCode :: Text -> Maybe ()
isPreferredCountryCode code = preferredCountryCodes ^? ix code
