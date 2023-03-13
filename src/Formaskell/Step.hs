--------------------------------------------------------------------------------
module Formaskell.Step
    ( Lines
    , Step (..)
    , makeStep
    ) where


--------------------------------------------------------------------------------
import Formaskell.Module ( Module, Lines )

--------------------------------------------------------------------------------
data Step = Step
    { stepName   :: String
    , stepFilter :: Lines -> Module -> Lines
    }

--------------------------------------------------------------------------------
makeStep :: String -> (Lines -> Module -> Lines) -> Step
makeStep = Step
