import Codec.Archive.Zip as Zip
import qualified Data.Set as Set
import Data.Text as Text
import qualified System.Directory as FS
import System.FilePath ((</>), (<.>))

import qualified Config
import qualified Peer
import qualified Server



preferredCountryCodes :: Set Text
preferredCountryCodes = Set.fromList [ "ch", "de", "gb", "nl", "se" ]



-- TODO: add some proper error handling.
main :: IO ()
main =
  do  putStrLn "Fetching current server list..."
      servers <- Server.fetchPreferred preferredCountryCodes

      putStrLn "Reading local peer list..."
      peers <- Peer.fromFile ".peers.json"

      currentPath <- FS.getCurrentDirectory

      forM_ peers $ \peer ->
        do  let clientName = unpack (Peer.peerName peer)
            let configPath = currentPath </> "configs" </> clientName

            putStrLn $ "Creating configs for " <> clientName <> "..."
            FS.createDirectoryIfMissing True configPath

            forM_ servers $ \server ->
              Config.writeToFile configPath $
                Config.create peer server

            putStrLn $ "Creating zip archive for " <> clientName <> "..."
            Zip.createArchive (configPath <.> "zip") $
              Zip.packDirRecur Zip.Deflate Zip.mkEntrySelector configPath

      putStrLn "Configurations created."
