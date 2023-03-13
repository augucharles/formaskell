--------------------------------------------------------------------------------
-- | There are a number of steps that sort items: 'Imports' and 'ModuleHeader',
-- and maybe more in the future.  This module provides consistent sorting
-- utilities.
{-# LANGUAGE LambdaCase #-}
module Formaskell.Ordering
    ( compareImports
    , compareLIE
    , compareWrappedName
    ) where


--------------------------------------------------------------------------------
import           Data.Char                    (isUpper, toLower)
import           Data.Function                (on)
import           Data.Ord                     (comparing)
import GHC.Hs
    ( GhcPs,
      LIE,
      IE(IEModuleContents, IEVar, IEThingAbs, IEThingAll, IEThingWith),
      ImportDecl(ideclName, ideclPkgQual),
      IEWrappedName )
import qualified GHC.Hs                       as GHC
import           GHC.Types.Name.Reader        (RdrName)
import           GHC.Types.SrcLoc             (unLoc)
import           GHC.Utils.Outputable         (Outputable)
import qualified GHC.Utils.Outputable         as GHC
import           Formaskell.GHC (showOutputable)


--------------------------------------------------------------------------------
-- | Compare imports for sorting.  Cannot easily be a lawful instance due to
-- case insensitivity.
compareImports
    :: GHC.ImportDecl GHC.GhcPs -> GHC.ImportDecl GHC.GhcPs -> Ordering
compareImports i0 i1 =
    ideclName i0 `compareOutputableCI` ideclName i1 <>
    showOutputable (ideclPkgQual i0) `compare`
        showOutputable (ideclPkgQual i1) <>
    compareOutputableCI i0 i1


--------------------------------------------------------------------------------
-- | NOTE: Can we get rid off this by adding a properly sorting newtype around
-- 'RdrName'?
compareLIE :: LIE GhcPs -> LIE GhcPs -> Ordering
compareLIE = comparing $ ieKey . unLoc
  where
    -- | The implementation is a bit hacky to get proper sorting for input specs:
    -- constructors first, followed by functions, and then operators.
    ieKey :: IE GhcPs -> (Int, String)
    ieKey = \case
        IEVar _ n            -> nameKey n
        IEThingAbs _ n       -> nameKey n
        IEThingAll _ n       -> nameKey n
        IEThingWith _ n _ _  -> nameKey n
        IEModuleContents _ n -> nameKey n
        _                    -> (2, "")


--------------------------------------------------------------------------------
compareWrappedName :: IEWrappedName RdrName -> IEWrappedName RdrName -> Ordering
compareWrappedName = comparing nameKey


--------------------------------------------------------------------------------
nameKey :: Outputable name => name -> (Int, String)
nameKey n = case showOutputable n of
    o@('(' : _)             -> (2, o)
    o@(o0 : _) | isUpper o0 -> (0, o)
    o                       -> (1, o)


--------------------------------------------------------------------------------
compareOutputableCI :: GHC.Outputable a => a -> a -> Ordering
compareOutputableCI = compare `on` (map toLower . showOutputable)
