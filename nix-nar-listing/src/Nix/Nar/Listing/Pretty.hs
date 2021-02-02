module Nix.Nar.Listing.Pretty where

import qualified Data.HashMap.Lazy as H
import           Data.Maybe
import qualified Data.Text.Lazy as T
import           Nix.Nar.Listing.Types
import           Prettyprinter
import           Prettyprinter.Render.Terminal

prettyNarListing :: NarListing -> Doc AnsiStyle
prettyNarListing NarListing{..} =
    prettyNarEntry (annotate (color Red) "<top>") narRoot

prettyNarEntry :: Doc AnsiStyle -> NarEntry -> Doc AnsiStyle
prettyNarEntry nameDoc EntryRegular{..} =
    annotate nameAnn nameDoc
    <+> if executable
        then annotate bold "(executable)"
        else mempty
    where
        executable = fromMaybe False entExecutable
        nameAnn = if executable then color Green else mempty

prettyNarEntry nameDoc EntryDirectory{..} =
    align . vsep $
        [ annotate (color Blue) nameDoc <> annotate bold "/"]
        ++ (uncurry go <$> H.toList entEntries)
    where
        go name ent = indent 2 $ prettyNarEntry (pretty name) ent

renderNarListing :: NarListing -> T.Text
renderNarListing =
    renderLazy
    . layoutPretty defaultLayoutOptions
    . prettyNarListing
