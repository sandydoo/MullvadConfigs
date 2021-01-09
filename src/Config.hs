{-# LANGUAGE NamedFieldPuns, OverloadedStrings, RecordWildCards #-}
module Config
  ( Config
  , create
  , writeToFile
  ) where


import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as UTF8
import Data.IP (IPRange)
import qualified Data.Set as Set
import           Data.Set (Set)
import Data.Text as Text
import System.FilePath ((</>), (<.>))

import Peer
import Server



-- Generate config


newtype Config = Config Text deriving (Show)


-- Explicitly send traffic for public IP ranges through the tunnel, excluding private / LAN ranges.
-- To instead send everything through the tunnel: 0.0.0.0/0,::0/0
publicIPRanges :: Set IPRange
publicIPRanges =
  Set.fromList
    [ "1.0.0.0/8", "2.0.0.0/8", "3.0.0.0/8", "4.0.0.0/6", "8.0.0.0/7", "11.0.0.0/8", "12.0.0.0/6", "16.0.0.0/4"
    , "32.0.0.0/3" , "64.0.0.0/2", "128.0.0.0/3", "160.0.0.0/5", "168.0.0.0/6", "172.0.0.0/12", "172.32.0.0/11"
    , "172.64.0.0/10", "172.128.0.0/9" , "173.0.0.0/8", "174.0.0.0/7", "176.0.0.0/4", "192.0.0.0/9", "192.128.0.0/11"
    , "192.160.0.0/13", "192.169.0.0/16", "192.170.0.0/15", "192.172.0.0/14", "192.176.0.0/12", "192.192.0.0/10"
    , "193.0.0.0/8", "194.0.0.0/7", "196.0.0.0/6", "200.0.0.0/5", "208.0.0.0/4", "193.138.218.74/32"
    , "::/0"
    ]


mullvadIPRange :: IPRange
mullvadIPRange = "10.64.0.0/10"



create :: Peer -> Server -> (Text, Config)
create Peer{ privateKey, ipv4Addr, ipv6Addr } server@Server{ publicKey, ipv4AddrIn } =
  let
    allowedIPs = Set.insert mullvadIPRange publicIPRanges

    serializeIPs :: Set IPRange -> Text
    serializeIPs ips =
      Set.foldr packIP "" ips

    packIP :: IPRange -> Text -> Text
    packIP ip ips
      | Text.null ips = serialize ip <> ips
      | otherwise     = serialize ip <> ", " <> ips

    serialize :: Show a => a -> Text
    serialize = Text.pack . show

    configName = Server.toPrettyName server

    config =
      Text.unlines $
        [ "[Interface]"
        , "PrivateKey = " <> privateKey
        , "Address = " <> serialize ipv4Addr <> "," <> serialize ipv6Addr
        , "DNS = 193.138.218.74"
        , ""
        , "[Peer]"
        , "PublicKey = " <> publicKey
        , "AllowedIPs = " <> serializeIPs allowedIPs
        , "Endpoint = " <> serialize ipv4AddrIn <> ":51820"
        ]
  in
  (configName, Config config)


writeToFile :: FilePath -> (Text, Config) -> IO ()
writeToFile directory (configName, Config config) =
  let
    filePath =
      directory </> unpack configName <.> "conf"

    toUTF8 =
      UTF8.fromString . unpack
  in
    BS.writeFile filePath (toUTF8 config)
