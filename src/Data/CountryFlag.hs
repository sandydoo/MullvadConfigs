module Data.CountryFlag
    ( fromCountryCode
    ) where

import qualified Data.Text as Text
import qualified TextBuilder as TB

fromCountryCode :: Text -> Text
fromCountryCode code =
    let
        toRegionalCode :: Char -> TB.TextBuilder
        toRegionalCode ch = TB.unicodeCodepoint $ ord ch - ord 'a' + ord '🇦'
     in
        TB.toText $
            Text.foldl' (\b ch -> b <> toRegionalCode ch) mempty code
