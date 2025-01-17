--------------------------------------------------------------------------------
-- | Utilities for assocgating comments with things in a list.
{-# LANGUAGE ImportQualifiedPost                       #-}
{-# LANGUAGE RecordWildCards                           #-}
{-# LANGUAGE ScopedTypeVariables                       #-}
module Hasklean.Comments
    ( CommentGroup(..)
    , commentGroupHasComments
    , commentGroupSort
    , commentGroups
    ) where


--------------------------------------------------------------------------------
import Data.Function                                   ( on )
import Data.List                                       ( sortBy, sortOn )
import Data.Maybe                                      ( isNothing, maybeToList )
import GHC.Hs qualified as GHC
import GHC.Types.SrcLoc qualified as GHC
import GHC.Utils.Outputable qualified as GHC


--------------------------------------------------------------------------------
import Hasklean.Block
    ( Block(blockStart)
    , LineBlock
    , adjacent
    , realSrcSpanToLineBlock
    )
import Hasklean.GHC                                    ( showOutputable )


--------------------------------------------------------------------------------
data CommentGroup a = CommentGroup
    { cgBlock     :: LineBlock
    , cgPrior     :: [GHC.LEpaComment]
    , cgItems     :: [(a, Maybe GHC.LEpaComment)]
    , cgFollowing :: [GHC.LEpaComment]
    }


--------------------------------------------------------------------------------
instance GHC.Outputable a => Show (CommentGroup a) where
    show CommentGroup {..} = "(CommentGroup (" ++
        show cgBlock ++ ") (" ++
        showOutputable cgPrior ++ ") (" ++
        showOutputable cgItems ++ ") (" ++
        showOutputable cgFollowing ++ "))"


--------------------------------------------------------------------------------
commentGroups
    :: forall a.
       (a -> Maybe GHC.RealSrcSpan)
    -> [a]
    -> [GHC.LEpaComment]
    -> [CommentGroup a]
commentGroups getSpan allItems allComments =
    work Nothing (sortOn fst allItemsWithLines) (sortOn fst commentsWithLines)
  where
    allItemsWithLines :: [(LineBlock, a)]
    allItemsWithLines = do
        item <- allItems
        s <- maybeToList $ getSpan item
        pure (realSrcSpanToLineBlock s, item)

    commentsWithLines :: [(LineBlock, GHC.LEpaComment)]
    commentsWithLines = do
        comment <- allComments
        let s = GHC.anchor $ GHC.getLoc comment
        pure (realSrcSpanToLineBlock s, comment)

    work
        :: Maybe (CommentGroup a)
        -> [(LineBlock, a)]
        -> [(LineBlock, GHC.LEpaComment)]
        -> [CommentGroup a]
    work mbCurrent items comments = case takeNext items comments of
        Nothing -> maybeToList mbCurrent
        Just (b, next, items', comments') ->
            let (flush, current) = case mbCurrent of
                    Just c  | adjacent (cgBlock c) b
                            , nextThingItem next
                            , following@(_ : _) <- cgFollowing c ->
                        ([c {cgFollowing = []}], CommentGroup b following [] [])
                    Just c  | adjacent (cgBlock c) b ->
                        ([], c {cgBlock = cgBlock c <> b})
                    _ -> (maybeToList mbCurrent, CommentGroup b [] [] [])
                current' = case next of
                    NextItem i -> current {cgItems = cgItems current <> [(i, Nothing)]}
                    NextComment c
                        | null (cgItems current) -> current {cgPrior = cgPrior current <> [c]}
                        | otherwise -> current {cgFollowing = cgFollowing current <> [c]}
                    NextItemWithComment i c ->
                        current {cgItems = cgItems current <> [(i, Just c)]} in
            flush ++ work (Just current') items' comments'



--------------------------------------------------------------------------------
takeNext
    :: [(LineBlock, a)]
    -> [(LineBlock, GHC.LEpaComment)]
    -> Maybe (LineBlock, NextThing a, [(LineBlock, a)], [(LineBlock, GHC.LEpaComment)])
takeNext [] [] = Nothing
takeNext [] ((cb, c) : comments) =
    Just (cb, NextComment c, [], comments)
takeNext ((ib, i) : items) [] =
    Just (ib, NextItem i, items, [])
takeNext ((ib, i) : items) ((cb, c) : comments)
    | blockStart ib == blockStart cb =
        Just (ib <> cb, NextItemWithComment i c, items, comments)
    | blockStart ib < blockStart cb =
        Just (ib, NextItem i, items, (cb, c) : comments)
    | otherwise =
        Just (cb, NextComment c, (ib, i) : items, comments)


--------------------------------------------------------------------------------
data NextThing a
    = NextComment GHC.LEpaComment
    | NextItem a
    | NextItemWithComment a GHC.LEpaComment


--------------------------------------------------------------------------------
instance GHC.Outputable a => Show (NextThing a) where
    show (NextComment c) = "NextComment " ++ showOutputable c
    show (NextItem i) = "NextItem " ++ showOutputable i
    show (NextItemWithComment i c) =
        "NextItemWithComment " ++ showOutputable i ++ " " ++ showOutputable c


--------------------------------------------------------------------------------
nextThingItem :: NextThing a -> Bool
nextThingItem (NextComment _)           = False
nextThingItem (NextItem _)              = True
nextThingItem (NextItemWithComment _ _) = True


--------------------------------------------------------------------------------
commentGroupHasComments :: CommentGroup a -> Bool
commentGroupHasComments CommentGroup {..} = not $
    null cgPrior && all (isNothing . snd) cgItems && null cgFollowing


--------------------------------------------------------------------------------
commentGroupSort :: (a -> a -> Ordering) -> CommentGroup a -> CommentGroup a
commentGroupSort cmp cg = cg
    { cgItems = sortBy (cmp `on` fst) (cgItems cg)
    }
