module Main where

import           Control.Monad (when)
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.Text.Lazy.IO as T
import           Nix.Nar.Listing.Pretty
import           Nix.Nar.Listing.Types
import           System.Environment
import           System.IO

main :: IO ()
main = do
    isTTY <- hIsTerminalDevice stdin
    progName <- getProgName
    when isTTY
        (hPutStrLn stderr $
            progName ++ ": Warning: Awaiting input from stdin on terminal...")
    contents <- C.getContents
    case parseNarListing contents of
        Left err -> hPutStrLn stderr $ "Failed to parse JSON: " ++ err
        Right val -> T.putStrLn $ renderNarListing val
