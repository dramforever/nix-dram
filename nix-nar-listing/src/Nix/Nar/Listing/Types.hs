{-# LANGUAGE TemplateHaskell #-}

module Nix.Nar.Listing.Types where

import           Data.Aeson
import           Data.Aeson.TH
import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.HashMap.Lazy as H
import qualified Data.Text as T
import           Nix.Nar.Listing.Utils

data NarListing =
    NarListing
    { narRoot :: NarEntry
    }
    deriving (Show)

data NarEntry
    = EntryRegular
        { entSize :: Integer
        , entNarOffset :: Integer
        , entExecutable :: Maybe Bool
        }
    | EntryDirectory
        { entEntries :: H.HashMap T.Text NarEntry
        }
    deriving (Show)

$(deriveJSON
    defaultOptions
        { sumEncoding = TaggedObject
            { tagFieldName = "type"
            , contentsFieldName = ""
            }
        , fieldLabelModifier = lowerFirst . drop 3
        , constructorTagModifier = lowerFirst . drop 5
        }
    ''NarEntry)

$(deriveJSON
    defaultOptions { fieldLabelModifier = lowerFirst . drop 3 }
    ''NarListing)

parseNarListing :: LC.ByteString -> Either String NarListing
parseNarListing = eitherDecode
