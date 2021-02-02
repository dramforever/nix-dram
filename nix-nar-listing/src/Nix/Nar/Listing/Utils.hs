module Nix.Nar.Listing.Utils where

import Data.Char

lowerFirst :: String -> String
lowerFirst "" = ""
lowerFirst (x : xs) = toLower x : xs
