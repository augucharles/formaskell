{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE DoAndIfThenElse   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use record patterns" #-}
module Formaskell.Step.Imports
  ( Options (..)
  , defaultOptions
  , EmptyListAlign (..)
  , GroupRule (..)
  , step

  , printImport

  , parsePattern
  , unsafeParsePattern
  ) where

--------------------------------------------------------------------------------
import           Control.Monad                     (forM_, when)
import qualified Data.Aeson                        as A
import           Data.Foldable                     (toList)
import           Data.Function                     (on, (&))
import           Data.Functor                      (($>))
import           Data.List                         (sortBy)
import           Data.List.NonEmpty                (NonEmpty (..))
import qualified Data.List.NonEmpty                as NonEmpty
import qualified Data.Map                          as Map
import           Data.Maybe                        (fromMaybe)
import qualified Data.Set                          as Set
import qualified Data.Text                         as T
import qualified GHC.Data.FastString               as GHC
import qualified GHC.Hs                            as GHC
import qualified GHC.Types.Name.Reader             as GHC
import qualified GHC.Types.PkgQual                 as GHC
import qualified GHC.Types.SourceText              as GHC
import qualified GHC.Types.SrcLoc                  as GHC
import qualified GHC.Unit.Module.Name              as GHC
import qualified GHC.Unit.Types                    as GHC
import qualified Text.Regex.TDFA                   as Regex
import           Text.Regex.TDFA                   (Regex)
import           Text.Regex.TDFA.ReadRegex         (parseRegex)

--------------------------------------------------------------------------------
import Formaskell.Block ( Block(Block) )
import qualified Formaskell.Editor   as Editor
import Formaskell.Module
    ( canMergeImport,
      importModuleName,
      mergeModuleImport,
      moduleImportGroups,
      Lines,
      Module )
import Formaskell.Ordering
    ( compareImports, compareLIE, compareWrappedName )
import Formaskell.Printer
    ( comma,
      getCurrentLine,
      modifyCurrentLine,
      newline,
      parenthesize,
      putRdrName,
      putText,
      runPrinter_,
      sep,
      space,
      wrapping,
      P,
      PrinterConfig(PrinterConfig) )
import Formaskell.Step ( makeStep, Step )
import Formaskell.Util ( flagEnds, trimRight )

--------------------------------------------------------------------------------
data Options = Options
    { alignColumn    :: Int
    , alignAs        :: Int
    , emptyListAlign :: EmptyListAlign
    , listPadding    :: Int
    , separateLists  :: Bool
    , spaceSurround  :: Bool
    , groupRules     :: [GroupRule]
    } deriving (Eq, Show)

defaultOptions :: Options
defaultOptions = Options
    { alignColumn    = 56
    , alignAs        = 4
    , emptyListAlign = Inherit
    , listPadding    = 4
    , separateLists  = False
    , spaceSurround  = True
    , groupRules     = [defaultGroupRule]
    }
  where defaultGroupRule = GroupRule
          { match    = unsafeParsePattern ".*"
          , subGroup = Just $ unsafeParsePattern "^[^.]+"
          }

data EmptyListAlign
    = Inherit
    | RightAfter
    deriving (Eq, Show)

-- | A rule for grouping imports that specifies which module names
-- belong in a group and (optionally) how to break them up into
-- sub-groups.
--
-- See the documentation for the group_rules setting in
-- data/formaskell.yaml for more details.
data GroupRule = GroupRule
  { match    :: Pattern
    -- ^ The pattern that determines whether a rule applies to a
    -- module name.
  , subGroup :: Maybe Pattern
    -- ^ An optional pattern for breaking the group up into smaller
    -- sub-groups.
  } deriving (Show, Eq)

instance A.FromJSON GroupRule where
  parseJSON = A.withObject "group_rule" parse
    where parse o = GroupRule
                <$> (o A..: "match")
                <*> (o A..:? "sub_group")

-- | A compiled regular expression. Provides instances that 'Regex'
-- does not have (eg 'Show', 'Eq' and 'FromJSON').
--
-- Construct with 'parsePattern' to maintain the invariant that
-- 'string' is the exact regex string used to compile 'regex'.
data Pattern = Pattern
  { regex  :: Regex
    -- ^ The compiled regular expression.
  , string :: String
    -- ^ The valid regex string that 'regex' was compiled from.
  }

instance Show Pattern where show = show . string

instance Eq Pattern where (==) = (==) `on` string

instance A.FromJSON Pattern where
  parseJSON = A.withText "regex" parse
    where parse text = case parsePattern $ T.unpack text of
            Left err  -> fail $ "Invalid regex:\n" <> err
            Right pat -> pure pat


-- | Parse a string into a compiled regular expression ('Pattern').
--
-- Returns a human-readable parse error message if the string is not
-- valid regex syntax.
--
-- >>> parsePattern "^([^.]+)"
-- Right "^([^.]+)"
--
-- >>> parsePattern "("
-- Left "\"(\" (line 1, column 2):\nunexpected end of input\nexpecting empty () or anchor ^ or $ or an atom"
parsePattern :: String -> Either String Pattern
parsePattern string = case parseRegex string of
  Right _  -> Right $ Pattern { string, regex = Regex.makeRegex string }
  Left err -> Left (show err)

-- | Parse a string into a regular expression, raising a runtime
-- exception if the string is not valid regex syntax.
--
-- >>> unsafeParsePattern "^([^.]+)"
-- "^([^.]+)"
--
-- >>> unsafeParsePattern "("
-- "*** Exception: "(" (line 1, column 2):
-- unexpected end of input
-- expecting empty () or anchor ^ or $ or an atom
unsafeParsePattern :: String -> Pattern
unsafeParsePattern = either error id . parsePattern

--------------------------------------------------------------------------------
step :: Maybe Int -> Options -> Step
step columns = makeStep "Imports (ghc-lib-parser)" . printImports columns


--------------------------------------------------------------------------------
printImports :: Maybe Int -> Options -> Lines -> Module -> Lines
printImports maxCols options ls m = Editor.apply changes ls
  where
    groups = moduleImportGroups m
    moduleStats = foldMap (importStats . GHC.unLoc) $ concatMap toList groups
    changes = foldMap (formatGroup maxCols options moduleStats) groups

formatGroup
    :: Maybe Int -> Options -> ImportStats
    -> NonEmpty (GHC.LImportDecl GHC.GhcPs) -> Editor.Edits
formatGroup maxCols options moduleStats imports =
    let newLines = formatImports maxCols options moduleStats imports in
    Editor.changeLines (importBlock imports) (const newLines)

importBlock :: NonEmpty (GHC.LImportDecl GHC.GhcPs)  -> Block String
importBlock group = Block
    (GHC.srcSpanStartLine . src $ NonEmpty.head group)
    (GHC.srcSpanEndLine   . src $ NonEmpty.last group)
  where
    src = fromMaybe (error "importBlock: missing location") .
        GHC.srcSpanToRealSrcSpan . GHC.getLocA

formatImports
    :: Maybe Int    -- ^ Max columns.
    -> Options      -- ^ Options.
    -> ImportStats  -- ^ Module stats.
    -> NonEmpty (GHC.LImportDecl GHC.GhcPs) -> Lines
formatImports maxCols options moduleStats rawGroup =
  runPrinter_ (PrinterConfig maxCols) do
  let
    group :: NonEmpty (GHC.LImportDecl GHC.GhcPs)
    group
      = NonEmpty.sortBy (compareImports `on` GHC.unLoc) rawGroup
      & mergeImports

    stats = moduleStats {isAnyQualified = True}

  forM_ group \imp -> printQualified options stats imp >> newline


--------------------------------------------------------------------------------
printQualified
    :: Options -> ImportStats -> GHC.LImportDecl GHC.GhcPs -> P ()
printQualified Options{..} stats ldecl = do
    putText "import" >> space

    case (isSource decl, isAnySource stats) of
      (True, _) -> putText "{-# SOURCE #-}" >> space
      (_, True) -> putText "              " >> space
      _         -> pure ()

    when (GHC.ideclSafe decl) (putText "safe" >> space)

    let module_ = do
            moduleNamePosition <- length <$> getCurrentLine
            case GHC.ideclPkgQual decl of
              GHC.NoRawPkgQual   -> pure ()
              GHC.RawPkgQual pkg -> putText (stringLiteral pkg) >> space
            putText (importModuleName decl)

            pure moduleNamePosition

    -- add "qualified" after module name
    _ <-
        if isQualified decl
          then module_ <* space <* putText "qualified"
          else module_

    let somethingFollows =
                isHiding decl ||
                not (null $ GHC.ideclHiding decl)

    -- put "as" on new line if there is an explicit import list, otherwise as follows module name
    case GHC.ideclAs decl of
      Nothing -> pure ()
      Just lname  -> (
        if somethingFollows then
          (do
            newline >> putText (replicate alignAs ' ') >> putText "as" >> space
            putText . GHC.moduleNameString $ GHC.unLoc lname
          )
        else
          space >> putText "as" >> space >> putText (GHC.moduleNameString $ GHC.unLoc lname))

    when (isHiding decl) (space >> putText "hiding")

    afterAliasPosition <- length <$> getCurrentLine

    -- Only print spaces if something follows.
    when somethingFollows $ putText $ replicate
        (alignColumn - afterAliasPosition - 2)
        ' '

    let putOffset = putText $ replicate listPadding ' '

    case snd <$> GHC.ideclHiding decl of
        Nothing -> pure ()
        Just limports | null (GHC.unLoc limports) -> case emptyListAlign of
            RightAfter -> modifyCurrentLine trimRight >> space >> putText "()"
            Inherit -> space >> putText "()"

        Just limports -> do
            let imports = GHC.unLoc limports
                printedImports = flagEnds $ -- [P ()]
                    printImport separateLists . GHC.unLoc <$>
                    prepareImportList imports

            -- Helper
            let doSpaceSurround = when spaceSurround space

            -- Try to put everything on one line.
            let printAsSingleLine = forM_ printedImports $ \(imp, start, end) -> do
                    when start $ putText "(" >> doSpaceSurround
                    imp
                    if end then doSpaceSurround >> putText ")" else comma >> space

            -- Try to put everything one by one, wrapping if that fails.
            _ <- pure $ forM_ printedImports $
                    \(imp, start, end) ->
                    wrapping
                       (do
                         if start then putText "(" >> doSpaceSurround else space
                         imp
                         if end then doSpaceSurround >> putText ")" else comma)
                      (do
                        if start && spaceSurround then
                                -- Only necessary if spaceSurround is enabled.
                                modifyCurrentLine trimRight
                        else pure ()
                        newline
                        _ <-
                        --case listAlign of
                          -- Print the much needed '('
                          if start  then putText "(" >> doSpaceSurround
                          -- Don't bother aligning if we're not in inline mode.
                          else pure ()
                        imp
                        if end then doSpaceSurround >> putText ")" else comma)

            -- Put everything on a separate line.  'spaceSurround' can be
            -- ignored.
            let printAsMultiLine = forM_ printedImports $ \(imp, start, end) -> do
                    when start $ modifyCurrentLine trimRight  -- We added some spaces.
                    newline
                    putOffset
                    if start then putText "( " else putText ", "
                    imp
                    when end $ newline >> putOffset >> putText ")"

            wrapping
                (space >> printAsSingleLine)
                printAsMultiLine
  where
    decl = GHC.unLoc ldecl



--------------------------------------------------------------------------------
printImport :: Bool -> GHC.IE GHC.GhcPs -> P ()
printImport _ (GHC.IEVar _ name) = do
    printIeWrappedName name
printImport _ (GHC.IEThingAbs _ name) = do
    printIeWrappedName name
printImport separateLists (GHC.IEThingAll _ name) = do
    printIeWrappedName name
    when separateLists space
    putText "(..)"
printImport _ (GHC.IEModuleContents _ modu) = do
    putText "module"
    space
    putText . GHC.moduleNameString $ GHC.unLoc modu
printImport separateLists (GHC.IEThingWith _ name wildcard imps) = do
    printIeWrappedName name
    when separateLists space
    let ellipsis = case wildcard of
          GHC.IEWildcard _position -> [putText ".."]
          GHC.NoIEWildcard         -> []
    parenthesize $
      sep (comma >> space) (ellipsis <> fmap printIeWrappedName imps)
printImport _ (GHC.IEGroup _ _ _ ) =
    error "Formaskell.Printer.Imports.printImportExport: unhandled case 'IEGroup'"
printImport _ (GHC.IEDoc _ _) =
    error "Formaskell.Printer.Imports.printImportExport: unhandled case 'IEDoc'"
printImport _ (GHC.IEDocNamed _ _) =
    error "Formaskell.Printer.Imports.printImportExport: unhandled case 'IEDocNamed'"


--------------------------------------------------------------------------------
printIeWrappedName :: GHC.LIEWrappedName GHC.RdrName -> P ()
printIeWrappedName lie = case GHC.unLoc lie of
    GHC.IEName      n -> putRdrName n
    GHC.IEPattern _ n -> putText "pattern" >> space >> putRdrName n
    GHC.IEType    _ n -> putText "type" >> space >> putRdrName n


mergeImports
    :: NonEmpty (GHC.LImportDecl GHC.GhcPs)
    -> NonEmpty (GHC.LImportDecl GHC.GhcPs)
mergeImports (x :| []) = x :| []
mergeImports (h :| (t : ts))
  | canMergeImport (GHC.unLoc h) (GHC.unLoc t) = mergeImports (mergeModuleImport h t :| ts)
  | otherwise = h :| mergeImportsTail (t : ts)
  where
    mergeImportsTail (x : y : ys)
      | canMergeImport (GHC.unLoc x) (GHC.unLoc y) = mergeImportsTail (mergeModuleImport x y : ys)
      | otherwise = x : mergeImportsTail (y : ys)
    mergeImportsTail xs = xs


--------------------------------------------------------------------------------
data ImportStats = ImportStats
    { isLongestImport :: !Int
    , isAnySource     :: !Bool
    , isAnyQualified  :: !Bool
    , isAnySafe       :: !Bool
    }

instance Semigroup ImportStats where
    l <> r = ImportStats
        { isLongestImport = isLongestImport l `max` isLongestImport r
        , isAnySource     = isAnySource     l ||    isAnySource     r
        , isAnyQualified  = isAnyQualified  l ||    isAnyQualified  r
        , isAnySafe       = isAnySafe       l ||    isAnySafe       r
        }

instance Monoid ImportStats where
    mappend = (<>)
    mempty  = ImportStats 0 False False False

importStats :: GHC.ImportDecl GHC.GhcPs -> ImportStats
importStats i =
    ImportStats (importModuleNameLength i) (isSource i) (isQualified i) (GHC.ideclSafe  i)

-- Computes length till module name, includes package name.
-- TODO: this should reuse code with the printer
importModuleNameLength :: GHC.ImportDecl GHC.GhcPs -> Int
importModuleNameLength imp =
    (case GHC.ideclPkgQual imp of
        GHC.NoRawPkgQual  -> 0
        GHC.RawPkgQual sl -> 1 + length (stringLiteral sl)) +
    length (importModuleName imp)


--------------------------------------------------------------------------------
stringLiteral :: GHC.StringLiteral -> String
stringLiteral sl = case GHC.sl_st sl of
    GHC.NoSourceText -> show . GHC.unpackFS $ GHC.sl_fs sl
    GHC.SourceText s -> s


--------------------------------------------------------------------------------
isQualified :: GHC.ImportDecl GHC.GhcPs -> Bool
isQualified = (/=) GHC.NotQualified . GHC.ideclQualified

isHiding :: GHC.ImportDecl GHC.GhcPs -> Bool
isHiding = maybe False fst . GHC.ideclHiding

isSource :: GHC.ImportDecl GHC.GhcPs -> Bool
isSource = (==) GHC.IsBoot . GHC.ideclSource


--------------------------------------------------------------------------------
-- | Cleans up an import item list.
--
-- * Sorts import items.
-- * Sort inner import lists, e.g. `import Control.Monad (Monad (return, join))`
-- * Removes duplicates from import lists.
prepareImportList :: [GHC.LIE GHC.GhcPs] -> [GHC.LIE GHC.GhcPs]
prepareImportList =
  sortBy compareLIE . map (fmap prepareInner) .
  concatMap (toList . snd) . Map.toAscList . mergeByName
 where
  mergeByName
      :: [GHC.LIE GHC.GhcPs]
      -> Map.Map GHC.RdrName (NonEmpty (GHC.LIE GHC.GhcPs))
  mergeByName imports0 = Map.fromListWith
    -- Note that ideally every NonEmpty will just have a single entry and we
    -- will be able to merge everything into that entry.  Exotic imports can
    -- mess this up, though.  So they end up in the tail of the list.
    (\(x :| xs) (y :| ys) -> case ieMerge (GHC.unLoc x) (GHC.unLoc y) of
      Just z  -> (x $> z) :| (xs ++ ys)  -- Keep source from `x`
      Nothing -> x :| (xs ++ y : ys))
    [(GHC.ieName $ GHC.unLoc imp, imp :| []) | imp <- imports0]

  prepareInner :: GHC.IE GHC.GhcPs -> GHC.IE GHC.GhcPs
  prepareInner = \case
    -- Simplify `A ()` to `A`.
    GHC.IEThingWith x n GHC.NoIEWildcard [] -> GHC.IEThingAbs x n
    GHC.IEThingWith x n w ns ->
      GHC.IEThingWith x n w (sortBy (compareWrappedName `on` GHC.unLoc) ns)
    ie -> ie

  -- Merge two import items, assuming they have the same name.
  ieMerge :: GHC.IE GHC.GhcPs -> GHC.IE GHC.GhcPs -> Maybe (GHC.IE GHC.GhcPs)
  ieMerge l@(GHC.IEVar _ _)      _                  = Just l
  ieMerge _                  r@(GHC.IEVar _ _)      = Just r
  ieMerge (GHC.IEThingAbs _ _)   r                  = Just r
  ieMerge l                  (GHC.IEThingAbs _ _)   = Just l
  ieMerge l@(GHC.IEThingAll _ _) _                  = Just l
  ieMerge _                  r@(GHC.IEThingAll _ _) = Just r
  ieMerge (GHC.IEThingWith x0 n0 w0 ns0) (GHC.IEThingWith _ _ w1 ns1)
    | w0 /= w1  = Nothing
    | otherwise = Just $
        -- TODO: sort the `ns0 ++ ns1`?
        GHC.IEThingWith x0 n0 w0 (nubOn GHC.lieWrappedName $ ns0 ++ ns1)
  ieMerge _ _ = Nothing


--------------------------------------------------------------------------------
nubOn :: Ord k => (a -> k) -> [a] -> [a]
nubOn f = go Set.empty
 where
  go _   []              = []
  go acc (x : xs)
    | y `Set.member` acc = go acc xs
    | otherwise          = x : go (Set.insert y acc) xs
   where
    y = f x