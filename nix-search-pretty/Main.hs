{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

import           Control.Applicative
import           Control.Monad (when)
import           Data.Aeson
import           Data.Aeson.TH
import qualified Data.ByteString.Lazy as L
import qualified Data.HashMap.Lazy as H
import           Data.Maybe
import qualified Data.Text as T
import qualified Data.Vector as V
import           Prettyprinter
import           Prettyprinter.Render.Terminal
import           Prettyprinter.Util
import           System.Environment
import           System.IO

newtype OneOrArray a = OneOrArray (V.Vector a)
    deriving (Show)

instance FromJSON1 OneOrArray where
    liftParseJSON _ go _ (Array arr) = OneOrArray <$> traverse go arr
    liftParseJSON _ go _ val = OneOrArray . V.singleton <$> go val

instance FromJSON a => FromJSON (OneOrArray a) where
    parseJSON = parseJSON1

data PackageLicense
    = PackageLicense
        { lic_fullName :: Maybe T.Text
        , lic_spdxId :: Maybe T.Text
        , lic_url :: Maybe T.Text
        }
    | PackageLicenseString T.Text
    deriving (Show)

$(deriveFromJSON
    defaultOptions
        { fieldLabelModifier = drop 4
        , sumEncoding = UntaggedValue
        }
    ''PackageLicense)

data PackageMeta =
    PackageMeta
    { meta_available :: Maybe Bool
    , meta_broken :: Maybe Bool
    , meta_insecure :: Maybe Bool
    , meta_unfree :: Maybe Bool
    , meta_unsupported :: Maybe Bool

    , meta_position :: Maybe T.Text

    , meta_homepage :: Maybe (OneOrArray T.Text)
    , meta_license :: Maybe (OneOrArray PackageLicense)
    }
    deriving (Show)

$(deriveFromJSON
    defaultOptions { fieldLabelModifier = drop 5 }
    ''PackageMeta)

data Package =
    Package
    { pkg_pname :: T.Text
    , pkg_version :: T.Text
    , pkg_description :: T.Text
    , pkg_meta :: PackageMeta
    }
    deriving (Show)

$(deriveFromJSON
    defaultOptions { fieldLabelModifier = drop 4 }
    ''Package)

prettyPackage :: T.Text -> Package -> Doc AnsiStyle
prettyPackage attr Package{..} =
    align . vsep $
    [ bullet
        <+> annotate underlined (pretty attr)
        <+> annotate (colorDull Blue) (parens (pretty pkg_pname))
    , indent 2 $ description
    , ""
    , indent 2 $ metaFlags
    , indent 2 $ homepage
    , indent 2 $ license
    , indent 2 $ defPos
    , ""
    ]
    where
        PackageMeta{..} = pkg_meta

        bullet = annotate bold $
            if fromMaybe True meta_available
                then annotate (color Green) "*"
                else annotate (color Red) "✗"

        description = case pkg_description of
            "" -> annotate (colorDull White) "(No description)"
            _ -> reflow pkg_description

        fillItem = fill 11

        homepage = case meta_homepage of
            Just (OneOrArray urls) ->
                fillItem (annotate bold "Homepage" <> ":")
                <+> annotate underlined (align . vsep $ pretty <$> V.toList urls)
            Nothing -> annotate (colorDull White) "(No homepage)"

        license = case meta_license of
            Just (OneOrArray licenses) ->
                fillItem (annotate bold "License" <> ":")
                <+> (align . vsep $ prettyLicense <$> V.toList licenses)
            Nothing -> annotate (colorDull White) "(No license)"

        prettyLicense PackageLicense{..} = namePart <> spdxPart
            where
                namePart = fromMaybe "Unknown" $
                    (pretty <$> lic_fullName)
                    <|> (annotate underlined . pretty <$> lic_url)
                spdxPart = case lic_spdxId of
                    Just spdx -> space <> (annotate bold $ parens (pretty spdx))
                    Nothing -> mempty
        prettyLicense (PackageLicenseString name) = pretty name

        defPos =
            fillItem (annotate bold "Defined at" <> ":")
            <+> annotate underlined (pretty meta_position)

        metaFlags = annotate bold $
            "["
            <+> flagSwitch meta_available
                (annotate (color Green) "✓ Available" <> " |")
                (annotate (color Red) "✗ Unavailable" <> " |")
            <> flagSwitch meta_unfree
                (space <> annotate (color Red) "Unfree")
                (space <> annotate (color Green) "Free")
            <> flag meta_broken (space <> annotate (color Red) "Broken")
            <> flag meta_unsupported (space <> annotate (color Red) "Unsupported")
            <> flag meta_insecure (space <> annotate (color Red) "Insecure")
            <+> "]"

        flagSwitch Nothing _ _ = mempty
        flagSwitch (Just True) t _ = t
        flagSwitch (Just False) _ e = e

        flag b s = if fromMaybe False b then s else mempty

main :: IO ()
main = do
    isTTY <- hIsTerminalDevice stdin
    progName <- getProgName
    when isTTY
        (hPutStrLn stderr $
            progName ++ ": Warning: Awaiting input from stdin on terminal...")

    inp <- L.getContents
    case eitherDecode inp of
        Left err -> hPutDoc stderr . vsep $
            [ annotate (bold <> color Red) "Failed to parse JSON"
            , pretty err
            ]
        Right res ->
            if H.null res
                then putDoc $ annotate (bold <> color Red) "Error" <> ":" <+> "no results for the given search term(s)!"
                else putDoc . vsep $ uncurry prettyPackage <$> H.toList res
