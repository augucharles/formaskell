{-# LANGUAGE ImportQualifiedPost                       #-}
{-# LANGUAGE LambdaCase                                #-}
{-# LANGUAGE RecordWildCards                           #-}
{-# OPTIONS_GHC -Wno-missing-fields                    #-}
-- | Utility functions for working with the GHC AST
module Hasklean.GHC
    ( -- * Standard settings
      baseDynFlags
      -- * Outputable operators
    , showOutputable
      -- * Deconstruction
    , epAnnComments
    ) where

--------------------------------------------------------------------------------
import Data.List                                       ( sortOn )
import GHC.Driver.Ppr qualified
    as GHC                                             ( showPpr )
import GHC.Driver.Session                              ( defaultDynFlags )
import GHC.Driver.Session qualified as GHC
import GHC.Hs qualified as GHC

import GHC.Types.SrcLoc qualified as GHC
import GHC.Utils.Outputable qualified as GHC
import Language.Haskell.GhclibParserEx.GHC.Settings.Config qualified as GHCEx

baseDynFlags :: GHC.DynFlags
baseDynFlags = defaultDynFlags GHCEx.fakeSettings GHCEx.fakeLlvmConfig

showOutputable :: GHC.Outputable a => a -> String
showOutputable = GHC.showPpr baseDynFlags

epAnnComments :: GHC.EpAnn a -> [GHC.LEpaComment]
epAnnComments GHC.EpAnnNotUsed = []
epAnnComments GHC.EpAnn {..}   = priorAndFollowing comments

priorAndFollowing :: GHC.EpAnnComments -> [GHC.LEpaComment]
priorAndFollowing = sortOn (GHC.anchor . GHC.getLoc) . \case
    GHC.EpaComments         {..} -> priorComments
    GHC.EpaCommentsBalanced {..} -> priorComments ++ followingComments
