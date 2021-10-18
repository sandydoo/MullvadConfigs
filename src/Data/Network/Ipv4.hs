{-# LANGUAGE TypeFamilies #-}
module Data.Network.Ipv4
  ( IpAddress
  , IpNetMask
  , IpBlock
  , parseIpAddress
  , parseIpBlock
  ) where


import Data.Aeson
import Data.Bits        ( shiftR, (.&.) )
import qualified Data.Text as T
import Foreign.Marshal  ( allocaBytes )
import Foreign.Ptr      ( castPtr )
import Foreign.Storable ( peek, pokeByteOff )
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Text.Show
import System.IO.Unsafe ( unsafePerformIO )



newtype IpAddress = IpAddress Word32
  deriving stock ( Generic )
  deriving newtype ( Eq, Ord, Num, Enum, Bounded, Real, Integral )


instance ToText IpAddress where
  toText = showIpAddress


instance Show IpAddress where
  show = T.unpack . toText


instance FromJSON IpAddress where
  parseJSON (String w) = either (fail . show) return $ parseIpAddress w
  parseJSON _ = mzero


instance ToJSON IpAddress where
  toJSON = String . showIpAddress


showIpAddress :: IpAddress -> Text
showIpAddress (IpAddress w) =
  show ((w `shiftR` 24) .&. 0xff) <> "." <>
  show ((w `shiftR` 16) .&. 0xff) <> "." <>
  show ((w `shiftR` 8)  .&. 0xff) <> "." <>
  show ( w              .&. 0xff)


parseIpAddress :: Text -> Either (ParseErrorBundle Text Void) IpAddress
parseIpAddress = runParser ipAddressParser "ipv4"


ipAddressParser :: Parsec Void Text IpAddress
ipAddressParser = do
  a <- word8 <* char '.'
  b <- word8 <* char '.'
  c <- word8 <* char '.'
  d <- word8
  pure $ IpAddress (packWord8ToWord32 a b c d)


packWord8ToWord32 :: Word8 -> Word8 -> Word8 -> Word8 -> Word32
packWord8ToWord32 a b c d =
  unsafePerformIO . allocaBytes 4 $ \p -> do
    pokeByteOff p 0 a
    pokeByteOff p 1 b
    pokeByteOff p 2 c
    pokeByteOff p 3 d
    peek (castPtr p)
{-# INLINE packWord8ToWord32 #-}



-- IP netmask


newtype IpNetMask = IpNetMask Word8
  deriving newtype (Eq, Ord, Show, Enum, Num, Bounded)


instance ToText IpNetMask where
  toText (IpNetMask w) = show (fromIntegral w :: Int)



-- IP block


data IpBlock = IpBlock !IpAddress !IpNetMask
  deriving (Eq, Ord)


instance ToText IpBlock where
  toText = showIpBlock


instance Show IpBlock where
  show = T.unpack . toText


instance FromJSON IpBlock where
  parseJSON (String w) = either (fail . show) return $ parseIpBlock w
  parseJSON _ = mzero


instance ToJSON IpBlock where
  toJSON = String . showIpBlock


showIpBlock :: IpBlock -> Text
showIpBlock (IpBlock base mask) = toText base <> "/" <> toText mask


parseIpBlock :: Text -> Either (ParseErrorBundle Text Void) IpBlock
parseIpBlock = runParser ipBlockParser "ipv4 block"


ipBlockParser :: Parsec Void Text IpBlock
ipBlockParser =
  IpBlock
    <$> ipAddressParser
    <*  char '/'
    <*> (fmap IpNetMask word8)


word8 :: (MonadParsec e s m, Token s ~ Char) => m Word8
word8 = do
  n <- L.decimal
  pure $ (fromIntegral (n :: Int)) .&. 0xff
{-# INLINE word8 #-}
