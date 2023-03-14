{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <&>" #-}
--------------------------------------------------------------------------------
module Hasklean
    ( -- * Run
      runSteps
      -- ** Helpers
    , findHaskellFiles
    , stepName
      -- * Config
    , module Hasklean.Config
      -- * Misc
    , makeVerbose
    , Step
    ) where


--------------------------------------------------------------------------------
import           Control.Monad                                    (foldM)
import           System.Directory                                 (doesDirectoryExist,
                                                                   doesFileExist,
                                                                   listDirectory)
import           System.FilePath                                  (takeExtension,
                                                                   (</>))
import           Hasklean.Config
import           Hasklean.Parse                                 ( parseModule )
import           Hasklean.Step                                  ( Lines, Step(Step, stepName) )
import Hasklean.Verbose ( makeVerbose )


--------------------------------------------------------------------------------
runStep :: Extensions -> Maybe FilePath -> Lines -> Step -> Either String Lines
runStep exts mfp ls = \case
  Step _name step ->
    step ls <$> parseModule exts mfp (unlines ls)


--------------------------------------------------------------------------------
runSteps ::
     Extensions
  -> Maybe FilePath
  -> [Step]
  -> Lines
  -> Either String Lines
runSteps exts mfp steps ls =
 foldM (runStep exts mfp) ls steps


--------------------------------------------------------------------------------
-- | Searches Haskell source files in any given folder recursively.
findHaskellFiles :: Bool -> [FilePath] -> IO [FilePath]
findHaskellFiles v fs = mapM (findFilesR v) fs >>= return . concat


--------------------------------------------------------------------------------
findFilesR :: Bool -> FilePath -> IO [FilePath]
findFilesR _ []   = return []
findFilesR v path = do
  doesFileExist path >>= \case
    True -> return [path]
    _    -> doesDirectoryExist path >>= \case
      True  -> findFilesRecursive path >>=
        return . filter (\x -> takeExtension x == ".hs")
      False -> do
        makeVerbose v ("Input folder does not exists: " <> path)
        findFilesR v []
  where
    findFilesRecursive :: FilePath -> IO [FilePath]
    findFilesRecursive = listDirectoryFiles findFilesRecursive

    listDirectoryFiles :: (FilePath -> IO [FilePath])
                       -> FilePath -> IO [FilePath]
    listDirectoryFiles go topdir = do
      ps <- listDirectory topdir >>=
        mapM (\x -> do
                 let dir = topdir </> x
                 doesDirectoryExist dir >>= \case
                   True  -> go dir
                   False -> return [dir])
      return $ concat ps
