{-# LANGUAGE OverloadedStrings #-}
module Data.CountryFlag
  ( fromCountryCode
  ) where


import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text



fromCountryCode :: Text -> Maybe Flag
fromCountryCode code = Map.lookup code countryFlags


type Flag = Text


countryFlags :: Map Text Flag
countryFlags =
  Map.fromList
    [ ( "ch", "🇨🇭" )
    , ( "de", "🇩🇪" )
    , ( "gb", "🇬🇧" )
    , ( "nl", "🇳🇱" )
    , ( "se", "🇸🇪" )
    ]
