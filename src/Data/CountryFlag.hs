module Data.CountryFlag
  ( fromCountryCode
  ) where


import qualified Text.Builder as TB
import qualified Data.Text as Text



fromCountryCode :: Text -> Text
fromCountryCode code =
  let
    toRegionalCode :: Char -> TB.Builder
    toRegionalCode ch = TB.unicodeCodePoint $ ord ch - ord 'a' + ord '🇦'
  in
  TB.run $
    Text.foldl' (\b ch -> b <> toRegionalCode ch) mempty code
