{-# LANGUAGE NamedFieldPuns, OverloadedStrings, RecordWildCards #-}
module Config
  ( createConfigFile
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


createConfig :: Peer -> Server -> Text
createConfig Peer{ privateKey, ipv4Addr, ipv6Addr } Server{ publicKey, ipv4AddrIn } =
  let
    allowedIPs = Set.insert mullvadIPRange publicIPRanges

    serializeIPs :: Set IPRange -> Text
    serializeIPs ips =
      Set.foldr packIP "" ips

    packIP :: IPRange -> Text -> Text
    packIP ip ips
      | Text.null ips = fromIPRange ip <> ips
      | otherwise     = fromIPRange ip <> ", " <> ips

    fromIPRange :: IPRange -> Text
    fromIPRange = Text.pack . show

    showText :: Show a => a -> Text
    showText = Text.pack . show
  in
  Text.unlines $
    [ "[Interface]"
    , "PrivateKey = " <> privateKey
    , "Address = " <> showText ipv4Addr <> "," <> showText ipv6Addr
    , "DNS = 193.138.218.74"
    , ""
    , "[Peer]"
    , "PublicKey = " <> publicKey
    -- Explicitly send traffic for public IP ranges through the tunnel, excluding private / LAN ranges.
    -- To send instead everything through tunnel: 0.0.0.0/0,::0/0
    , "AllowedIPs = " <> serializeIPs allowedIPs
    , "Endpoint = " <> showText ipv4AddrIn <> ":51820"
    ]


createConfigFile :: Peer -> Server -> FilePath -> IO ()
createConfigFile peer server configPath =
  let
    newConfig =
      createConfig peer server

    toUTF8 =
      UTF8.fromString . unpack

    filePath =
      configPath </> unpack (createName server) <.> "conf"
  in
    BS.writeFile filePath (toUTF8 newConfig)
