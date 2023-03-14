--------------------------------------------------------------------------------
{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant <$>" #-}
module Hasklean.Config
    ( Extensions
    , Config (..)
    , ExitCodeBehavior (..)
    , defaultConfigBytes
    , loadConfig
    ) where


--------------------------------------------------------------------------------
import           Control.Monad                                    (forM, mzero)
import           Data.Aeson                                       (FromJSON (..))
import qualified Data.Aeson                                       as A
import qualified Data.Aeson.Types                                 as A
import qualified Data.ByteString                                  as B
import           Data.ByteString.Lazy                             (fromStrict)
import qualified Data.FileEmbed                                   as FileEmbed
import           Data.List                                        (intercalate,
                                                                   nub)
import           Data.Map                                         (Map)
import qualified Data.Map                                         as M
import           Data.Maybe                                       (fromMaybe)
import           Data.YAML                                        (prettyPosWithSource)
import           Data.YAML.Aeson                                  (decode1Strict)
import qualified System.IO                                        as IO (Newline (..),
                                                                         nativeNewline)


--------------------------------------------------------------------------------
import Hasklean.Step ( Step )
import qualified Hasklean.Step.Imports            as Imports
import qualified Hasklean.Step.LanguagePragmas    as LanguagePragmas
import qualified Hasklean.Step.ModuleHeader       as ModuleHeader
import qualified Hasklean.Step.TrailingWhitespace as TrailingWhitespace
import Hasklean.Verbose ( Verbose )
import Data.Char (toLower)


--------------------------------------------------------------------------------
type Extensions = [String]


--------------------------------------------------------------------------------
data Config = Config
    { configSteps              :: [Step]
    , configColumns            :: Maybe Int
    , configLanguageExtensions :: [String]
    , configNewline            :: IO.Newline
    , configExitCode           :: ExitCodeBehavior
    }

--------------------------------------------------------------------------------
data ExitCodeBehavior
  = NormalExitBehavior
  | ErrorOnFormatExitBehavior
  deriving (Eq)

instance Show ExitCodeBehavior where
  show NormalExitBehavior        = "normal"
  show ErrorOnFormatExitBehavior = "error_on_format"

--------------------------------------------------------------------------------
instance FromJSON Config where
    parseJSON = parseConfig


--------------------------------------------------------------------------------
defaultConfigBytes :: B.ByteString
defaultConfigBytes = $(FileEmbed.embedFile "data/hasklean.yaml")


--------------------------------------------------------------------------------
configFilePath :: Verbose -> Maybe FilePath -> IO (Maybe FilePath)
configFilePath _ (Just userSpecified) = return (Just userSpecified)
configFilePath _ Nothing              = return Nothing

--------------------------------------------------------------------------------
loadConfig :: Verbose -> Maybe FilePath -> IO Config
loadConfig verbose userSpecified = do
    mbFp <- configFilePath verbose userSpecified
    verbose $ "Loading configuration at " ++ fromMaybe "<embedded>" mbFp
    bytes <- maybe (return defaultConfigBytes) B.readFile mbFp
    case decode1Strict bytes of
        Left (pos, err)     -> error $ prettyPosWithSource pos (fromStrict bytes) ("Hasklean.Config.loadConfig: " ++ err)
        Right config -> do
          return $ config
            { configLanguageExtensions = nub $
                configLanguageExtensions config
            }

--------------------------------------------------------------------------------
parseConfig :: A.Value -> A.Parser Config
parseConfig (A.Object o) = do
    -- First load the config without the actual steps
    config <- Config
        <$> pure []
        <*> (o A..:! "columns"             A..!= Just 120)
        <*> (o A..:? "language_extensions" A..!= [])
        <*> (o A..:? "newline"             >>= parseEnum newlines IO.nativeNewline)
        <*> (o A..:? "exit_code"           >>= parseEnum exitCodes NormalExitBehavior)

    -- Then fill in the steps based on the partial config we already have
    stepValues <- o A..: "steps" :: A.Parser [A.Value]
    steps      <- mapM (parseSteps config) stepValues
    return config {configSteps = concat steps}
  where
    newlines =
        [ ("native", IO.nativeNewline)
        , ("lf",     IO.LF)
        , ("crlf",   IO.CRLF)
        ]
    exitCodes =
        [ ("normal", NormalExitBehavior)
        , ("error_on_format", ErrorOnFormatExitBehavior)
        ]
parseConfig _            = mzero


--------------------------------------------------------------------------------
catalog :: Map String (Config -> A.Object -> A.Parser Step)
catalog = M.fromList
    [ ("imports",             parseImports)
    , ("module_header",       parseModuleHeader)
    , ("language_pragmas",    parseLanguagePragmas)
    , ("trailing_whitespace", parseTrailingWhitespace)
    ]


--------------------------------------------------------------------------------
parseSteps :: Config -> A.Value -> A.Parser [Step]
parseSteps config val = do
    map' <- parseJSON val :: A.Parser (Map String A.Value)
    forM (M.toList map') $ \(k, v) -> case (M.lookup k catalog, v) of
        (Just parser, A.Object o) -> parser config o
        _                         -> fail $ "Invalid declaration for " ++ k


--------------------------------------------------------------------------------
-- | Utility for enum-like options
parseEnum :: [(String, a)] -> a -> Maybe String -> A.Parser a
parseEnum _    def Nothing  = return def
parseEnum strs _   (Just k) = case lookup k strs of
    Just v  -> return v
    Nothing -> fail $ "Unknown option: " ++ k ++ ", should be one of: " ++
        intercalate ", " (map fst strs)

--------------------------------------------------------------------------------
parseModuleHeader :: Config -> A.Object -> A.Parser Step
parseModuleHeader config o = fmap (ModuleHeader.step columns) $ ModuleHeader.Config
    <$> (o A..:? "indent"         A..!= ModuleHeader.indent        def)
    <*> (o A..:? "sort"           A..!= ModuleHeader.sort          def)
    <*> (o A..:? "separate_lists" A..!= ModuleHeader.separateLists def)
    <*> (o A..:? "break_where"      >>= parseEnum breakWhere (ModuleHeader.breakWhere def))
    <*> (o A..:? "open_bracket"     >>= parseEnum openBracket (ModuleHeader.openBracket def))
  where
    def = ModuleHeader.defaultConfig

    columns = configColumns config

    breakWhere =
        [ ("exports", ModuleHeader.Exports)
        , ("single",  ModuleHeader.Single)
        , ("inline",  ModuleHeader.Inline)
        , ("always",  ModuleHeader.Always)
        ]

    openBracket =
        [ ("same_line", ModuleHeader.SameLine)
        , ("next_line", ModuleHeader.NextLine)
        ]


--------------------------------------------------------------------------------
parseImports :: Config -> A.Object -> A.Parser Step
parseImports config o = fmap (Imports.step columns) $ Imports.Options
      <$> o A..:? "align_column" A..!= def Imports.alignColumn
      <*> o A..:? "align_as" A..!= def Imports.alignAs
      <*> (o A..:? "empty_list_align" >>= parseEnum emptyListAligns (def Imports.emptyListAlign))
      -- Note that padding has to be at least 1. Default is 4.
      <*> o A..:? "list_padding" A..!= def Imports.listPadding
      <*> o A..:? "separate_lists" A..!= def Imports.separateLists
      <*> o A..:? "space_surround" A..!= def Imports.spaceSurround
      <*> o A..:? "group_rules" A..!= def Imports.groupRules
  where
    def f = f Imports.defaultOptions

    columns = configColumns config

    emptyListAligns =
        [ ("inherit", Imports.Inherit)
        , ("right_after", Imports.RightAfter)
        ]


--------------------------------------------------------------------------------
parseLanguagePragmas :: Config -> A.Object -> A.Parser Step
parseLanguagePragmas config o = LanguagePragmas.step
    <$> pure (configColumns config)
    <*> (o A..:? "style" >>= parseEnum styles LanguagePragmas.Vertical)
    <*> o A..:? "align"            A..!= True
    <*> o A..:? "align_column"     A..!= 56
    <*> mkLanguage o
  where
    styles =
        [ ("vertical",         LanguagePragmas.Vertical)
        , ("compact",          LanguagePragmas.Compact)
        ]


--------------------------------------------------------------------------------
-- | Utilities for validating language prefixes
mkLanguage :: A.Object -> A.Parser String
mkLanguage o = do
    lang <- o A..:? "language_prefix"
    maybe (pure "LANGUAGE") validate lang
    where
        validate :: String -> A.Parser String
        validate s
            | fmap toLower s == "language" = pure s
            | otherwise = fail "please provide a valid language prefix"


--------------------------------------------------------------------------------
parseTrailingWhitespace :: Config -> A.Object -> A.Parser Step
parseTrailingWhitespace _ _ = return TrailingWhitespace.step
