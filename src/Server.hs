{-# LANGUAGE DeriveGeneric, StrictData, TemplateHaskell #-}
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
import qualified Data.Network.Ipv4 as Ipv4
import qualified Data.Network.Ipv6 as Ipv6
import Data.CountryFlag as CountryFlag



filterRawServerList :: AsValue s => Set Text -> s -> [ Server ]
filterRawServerList preferredCountryCodes rawServerList =
  let
    byPreferredCountryCode :: AsValue s => s -> Bool
    byPreferredCountryCode server =
      isJust $
        do code <- server ^? key "country_code" . _String
           preferredCountryCodes ^? ix code

  in
  rawServerList
    ^.. values
    . filteredBy ( key "type"   . _String . only "wireguard" )
    . filteredBy ( key "active" . _Bool   . only True )
    . filtered byPreferredCountryCode
    . _JSON


fetchPreferred :: Set Text -> IO [ Server ]
fetchPreferred preferredCountryCodes =
  do  request  <- HTTP.parseRequest "https://api.mullvad.net/www/relays/all/"
      response <- HTTP.getResponseBody <$> HTTP.httpBS request

      return $
        filterRawServerList preferredCountryCodes response



data Server =
  Server
    { serverHostname         :: Text
    , serverCountryCode      :: Text
    , serverCountryName      :: Text
    , serverCityCode         :: Text
    , serverCityName         :: Text
    , serverActive           :: Bool
    , serverOwned            :: Bool
    , serverProvider         :: Text
    , serverIpv4AddrIn       :: Ipv4.IpAddress
    , serverIpv6AddrIn       :: Ipv6.IpAddress
    , serverNetworkPortSpeed :: Int
    , serverType             :: Text
    , serverPublicKey        :: Text
    , serverMultihopPort     :: Port
    , serverSocksName        :: Text
    , serverStatusMessages   :: [ Text ]
    } deriving ( Generic, Show )


$( deriveToJSON defaultOptions ''Server )


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
        <*> o .: "network_port_speed"
        <*> o .: "type"
        <*> o .: "pubkey"
        <*> o .: "multihop_port"
        <*> o .: "socks_name"
        <*> o .: "status_messages"



toPrettyName :: Server -> Text
toPrettyName Server {..} =
  Text.intercalate "-" . catMaybes $
    [ Just $
        CountryFlag.fromCountryCode serverCountryCode
    , Just $
        Text.toLower serverCityName
    , Just $
        Text.takeWhile ( /= '-' ) serverHostname
    , if serverNetworkPortSpeed >= 10
      then Just "⚡"
      else Nothing
    , if serverOwned
      then Just "🌟"
      else Nothing
    ]
