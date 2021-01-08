module Main where


import Codec.Archive.Zip as Zip
import Control.Monad (forM_)
import Data.Text as Text
import qualified Network.HTTP.Simple as HTTP
import qualified System.Directory as FS
import System.FilePath ((</>), (<.>))

import ProcessConfigs



serverListURL :: String
serverListURL = "https://api.mullvad.net/www/relays/all/"


-- TODO: add some proper error handling.
main :: IO ()
main =
  do  putStrLn "Fetching current server list..."

      request <- HTTP.parseRequest serverListURL
      serverList <- return . filterServerList . HTTP.getResponseBody =<< HTTP.httpBS request

      putStrLn "Reading local peer list..."
      peers <- readPeerInfo

      currentPath <- FS.getCurrentDirectory

      forM_ peers $ \peer ->
        do  let name = unpack $ peerName peer
            let configPath = currentPath </> "configs" </> name

            putStrLn $ "Creating configs for " <> name <> "..."
            FS.createDirectoryIfMissing True configPath

            forM_ serverList $ \server ->
              createConfigFile peer server configPath

            putStrLn $ "Creating zip archive for " <> name <> "..."
            Zip.createArchive (configPath <.> "zip") $
              Zip.packDirRecur Zip.Deflate Zip.mkEntrySelector configPath

      putStrLn "Configurations created."
