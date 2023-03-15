--------------------------------------------------------------------------------
module Hasklean.Step.Tabs
    ( step
    ) where


--------------------------------------------------------------------------------
import Hasklean.Step                                   ( Step, makeStep )


--------------------------------------------------------------------------------
removeTabs :: Int -> String -> String
removeTabs spaces = concatMap removeTabs'
  where
    removeTabs' '\t' = replicate spaces ' '
    removeTabs' x    = [x]


--------------------------------------------------------------------------------
step :: Int -> Step
step spaces = makeStep "Tabs" $ \ls _ -> map (removeTabs spaces) ls
