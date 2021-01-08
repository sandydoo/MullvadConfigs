module Main where


import Codec.Archive.Zip as Zip
import Control.Monad (forM_)
import Data.Text as Text
import qualified Network.HTTP.Simple as HTTP
import qualified System.Directory as FS
import System.FilePath ((</>), (<.>))

import Config
import Peer   as Peer
import Server as Server



serverListURL :: String
serverListURL = "https://api.mullvad.net/www/relays/all/"


-- TODO: add some proper error handling.
main :: IO ()
main =
  do  putStrLn "Fetching current server list..."

      request <- HTTP.parseRequest serverListURL
      serverList <- return . filterServerList . HTTP.getResponseBody =<< HTTP.httpBS request

      putStrLn "Reading local peer list..."
      peers <- Peer.fromFile ".peers.json"

      currentPath <- FS.getCurrentDirectory

      forM_ peers $ \peer ->
        do  let clientName = unpack (Peer.name peer)
            let configPath = currentPath </> "configs" </> clientName

            putStrLn $ "Creating configs for " <> clientName <> "..."
            FS.createDirectoryIfMissing True configPath

            forM_ serverList $ \server ->
              Config.createAndWriteToFile configPath peer server

            putStrLn $ "Creating zip archive for " <> clientName <> "..."
            Zip.createArchive (configPath <.> "zip") $
              Zip.packDirRecur Zip.Deflate Zip.mkEntrySelector configPath

      putStrLn "Configurations created."
