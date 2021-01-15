{-# LANGUAGE DeriveGeneric, TemplateHaskell #-}
module Server
  ( Server(..)
  , fetchPreferred
  , toPrettyName
  ) where


import Control.Lens
import Data.Aeson
import Data.Aeson.Lens
import Data.Aeson.TH
import Data.Text as Text
import qualified Network.HTTP.Simple as HTTP

import Data.Network
import Data.CountryFlag as CountryFlag



filterRawServerList :: AsValue s => Set Text -> s -> [Server]
filterRawServerList preferredCountryCodes rawServerList =
  let
    isPreferredCountry :: Text -> Maybe ()
    isPreferredCountry code = preferredCountryCodes ^? ix code

    countryCode :: AsValue s => s -> Maybe Text
    countryCode server = server ^? key "country_code" . _String
  in
  rawServerList
    ^.. values
    . filteredBy (key "type"   . _String . only "wireguard")
    . filteredBy (key "active" . _Bool   . only True)
    . filtered (\server -> isJust $ isPreferredCountry =<< countryCode server)
    . _JSON


fetchPreferred :: Set Text -> IO [Server]
fetchPreferred preferredCountryCodes =
  do  request <- HTTP.parseRequest "https://api.mullvad.net/www/relays/all/"
      response <- HTTP.getResponseBody <$> HTTP.httpBS request

      return $ filterRawServerList preferredCountryCodes response



data Server =
  Server
    { sHostname     :: Text
    , sCountryCode  :: Text
    , sCountryName  :: Text
    , sCityCode     :: Text
    , sCityName     :: Text
    , sActive       :: Bool
    , sOwned        :: Bool
    , sProvider     :: Text
    , sIpv4AddrIn   :: IPv4
    , sIpv6AddrIn   :: IPv6
    , sServerType   :: Text
    , sPublicKey    :: Text
    , sMultihopPort :: PortNumber
    , sSocksName    :: Text
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



toPrettyName :: Server -> Text
toPrettyName Server{..} =
  let
    lowerCityName = Text.toLower sCityName

    serverCode = Text.takeWhile (/= '-') sHostname

    maybeCountryEmoji = CountryFlag.fromCountryCode sCountryCode

    maybePreferredServer = if sOwned then Just "🌟" else Nothing

    nameList =
      [ maybeCountryEmoji, Just lowerCityName, Just serverCode, maybePreferredServer ]

  in
    Text.intercalate "-" . catMaybes $ nameList
