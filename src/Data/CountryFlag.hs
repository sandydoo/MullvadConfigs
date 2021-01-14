module Data.CountryFlag
  ( fromCountryCode
  ) where


import qualified Data.Map as Map



type Flag = Text


fromCountryCode :: Text -> Maybe Flag
fromCountryCode code = Map.lookup code countryFlags


countryFlags :: Map Text Flag
countryFlags =
  Map.fromList
    [ ( "ch", "🇨🇭" )
    , ( "de", "🇩🇪" )
    , ( "gb", "🇬🇧" )
    , ( "nl", "🇳🇱" )
    , ( "se", "🇸🇪" )
    ]
