--------------------------------------------------------------------------------
{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE OverloadedStrings #-}
module Formaskell.Step.LanguagePragmas
    ( Style (..)
    , step
    ) where


--------------------------------------------------------------------------------
import           Data.List.NonEmpty              (NonEmpty, fromList, toList)
import qualified Data.Set                        as S


--------------------------------------------------------------------------------
import qualified GHC.Types.SrcLoc                as GHC


--------------------------------------------------------------------------------
import Formaskell.Block ( Block(Block), groupAdjacent )
import qualified Formaskell.Editor as Editor
import Formaskell.Module
    ( Module, Lines, unsafeModuleAllPragmas )
import Formaskell.Step ( Step, makeStep )
import Formaskell.Util ( padRight, wrapMaybe )


--------------------------------------------------------------------------------
data Style
    = Vertical
    | Compact
    deriving (Eq, Show)


--------------------------------------------------------------------------------
verticalPragmas :: String -> Int -> Bool -> Int -> [String] -> Lines
verticalPragmas lg longest align alignColumn pragmas' =
    [ "{-#" ++ pad pragma ++ "#-}"
    | pragma <- pragmas'
    ]
  where
    pad
      | align = padRight $ (longest `max` alignColumn) - length lg - 4
      | otherwise = id


--------------------------------------------------------------------------------
compactPragmas :: String -> Maybe Int -> [String] -> Lines
compactPragmas lg columns pragmas' = wrapMaybe columns ("{-# " ++ lg) 13 $
    map (++ ",") (init pragmas') ++ [last pragmas' ++ " #-}"]


--------------------------------------------------------------------------------
prettyPragmas :: String -> Maybe Int -> Int -> Bool -> Int -> Style -> [String] -> Lines
prettyPragmas  lp _    longest align alignColumn Vertical        = verticalPragmas lp longest align alignColumn
prettyPragmas  lp cols _       _     _           Compact         = compactPragmas lp cols


--------------------------------------------------------------------------------
-- | Filter redundant (and duplicate) pragmas out of the groups. As a side
-- effect, we also sort the pragmas in their group...
filterRedundant ::
                   [(l, NonEmpty String)]
                -> [(l, [String])]
filterRedundant = snd . foldr filterRedundant' (S.empty, []) . fmap (fmap toList)
  where
    filterRedundant' (l, xs) (known, zs)
        | S.null xs' = (known', zs)
        | otherwise  = (known', (l, S.toAscList xs') : zs)
      where
        xs'    = S.fromList xs `S.difference` known
        known' = xs' `S.union` known

--------------------------------------------------------------------------------
step :: Maybe Int -> Style -> Bool -> Int -> String -> Step
step = ((((makeStep "LanguagePragmas" .) .) .) .) . step'

--------------------------------------------------------------------------------
step' :: Maybe Int -> Style -> Bool -> Int -> String -> Lines -> Module -> Lines
step' columns style align alignColumn _ ls m
--  | null languagePragmas && null otherPragmas = ls
  | null allPragmas = ls
  | otherwise = Editor.apply changes ls
  where

    --languagePragmas = moduleLanguagePragmas m
    allPragmas = unsafeModuleAllPragmas m

    convertFstToBlock :: [(GHC.RealSrcSpan, a)] -> [(Block String, a)]
    convertFstToBlock = fmap \(rspan, a) ->
      (Block (GHC.srcSpanStartLine rspan) (GHC.srcSpanEndLine rspan), a)

    groupAdjacent' =
      fmap turnSndBackToNel . groupAdjacent . fmap (fmap toList)
      where
        turnSndBackToNel (a, bss) = (a, fromList . concat $ bss)

    {- longest :: Int
    longest  = maximum $ map length $ toList . snd =<< languagePragmas -}

    longestAll :: Int
    longestAll = maximum $ map length $ toList . snd =<< allPragmas

    {- groups :: [(Block String, NonEmpty String)]
    groups = [(b, pgs) | (b, pgs) <- groupAdjacent' (convertFstToBlock languagePragmas)] -}

    groupsAll :: [(Block String, NonEmpty String)]
    groupsAll = [(b, pgs) | (b, pgs) <- groupAdjacent' (convertFstToBlock allPragmas)]

    changes = {- mconcat
      [ Editor.changeLines b (const $ prettyPragmas lngPrefix columns (longest `max` longestOther) align alignColumn style pg)
      | (b, pg) <- filterRedundant groups
      ]  <> -} mconcat
      [ Editor.changeLines b (const $ prettyPragmas "" columns longestAll align alignColumn style pg)
      | (b, pg) <- filterRedundant groupsAll
      ]
