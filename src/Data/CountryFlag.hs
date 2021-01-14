module Data.CountryFlag
  ( fromCountryCode
  ) where


import qualified Data.Map as Map



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
