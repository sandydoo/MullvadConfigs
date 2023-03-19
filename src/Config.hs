{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Config
    ( Config
    , create
    , writeToFile
    )
where

import qualified Data.ByteString as BS
import Data.Network
import qualified Data.Set as Set
import Data.Text as Text hiding (map)
import Peer
import Server
import System.FilePath ((<.>), (</>))

-- Generate config

newtype Config = Config Text deriving (Show)

-- Explicitly send traffic for public IP blocks through the tunnel, excluding private / LAN ranges.
-- To instead send everything through the tunnel: 0.0.0.0/0,::0/0
--
-- Internal Mullvad IP ranges: ~ 10.64.0.0/10
-- DNS and SOCKS5: 10.64.0.1
-- SOCKS5 to other servers: 10.124.x.x
publicIpBlocks :: Set IpBlock
publicIpBlocks =
    Set.fromList
        [ "1.0.0.0/8"
        , "2.0.0.0/7"
        , "4.0.0.0/6"
        , "8.0.0.0/5"
        , "16.0.0.0/4"
        , "32.0.0.0/3"
        , "64.0.0.0/3"
        , "96.0.0.0/4"
        , "112.0.0.0/5"
        , "120.0.0.0/6"
        , "124.0.0.0/7"
        , "126.0.0.0/8"
        , "128.0.0.0/3"
        , "160.0.0.0/5"
        , "168.0.0.0/8"
        , "169.0.0.0/9"
        , "169.128.0.0/10"
        , "169.192.0.0/11"
        , "169.224.0.0/12"
        , "169.240.0.0/13"
        , "169.248.0.0/14"
        , "169.252.0.0/15"
        , "169.255.0.0/16"
        , "170.0.0.0/7"
        , "172.0.0.0/12"
        , "172.32.0.0/11"
        , "172.64.0.0/10"
        , "172.128.0.0/9"
        , "173.0.0.0/8"
        , "174.0.0.0/7"
        , "176.0.0.0/4"
        , "192.0.0.0/9"
        , "192.128.0.0/11"
        , "192.160.0.0/13"
        , "192.169.0.0/16"
        , "192.170.0.0/15"
        , "192.172.0.0/14"
        , "192.176.0.0/12"
        , "192.192.0.0/10"
        , "193.0.0.0/8"
        , "194.0.0.0/7"
        , "196.0.0.0/6"
        , "200.0.0.0/5"
        , "208.0.0.0/4"
        , "224.0.0.0/4"
        , "::/1"
        , "8000::/2"
        , "c000::/3"
        , "e000::/4"
        , "f000::/5"
        , "f800::/6"
        , "fe00::/9"
        , "fec0::/10"
        , "ff00::/8"
        ]

create :: Peer -> Server -> (Text, Config)
create Peer {..} server@Server {..} =
    let allowedIps = publicIpBlocks

        serializeIps :: Set IpBlock -> Text
        serializeIps = Text.intercalate ", " . map toText . Set.toList

        configName = Server.toPrettyName server

        config =
            Text.unlines
                [ "[Interface]"
                , "PrivateKey = " <> peerPrivateKey
                , "Address = " <> toText peerIpv4Addr <> "," <> toText peerIpv6Addr
                , "DNS = 10.64.0.1"
                , ""
                , "[Peer]"
                , "PublicKey = " <> serverPublicKey
                , "AllowedIPs = " <> serializeIps allowedIps
                , "Endpoint = " <> toText serverIpv4AddrIn <> ":51820"
                ]
     in (configName, Config config)

writeToFile :: FilePath -> (Text, Config) -> IO ()
writeToFile directory (configName, Config config) =
    let filePath = directory </> unpack configName <.> "conf"
     in BS.writeFile filePath (encodeUtf8 config)
