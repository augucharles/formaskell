--------------------------------------------------------------------------------
module Hasklean.Step
    ( Lines
    , Step (..)
    , makeStep
    ) where


--------------------------------------------------------------------------------
import Hasklean.Module ( Module, Lines )

--------------------------------------------------------------------------------
data Step = Step
    { stepName   :: String
    , stepFilter :: Lines -> Module -> Lines
    }

--------------------------------------------------------------------------------
makeStep :: String -> (Lines -> Module -> Lines) -> Step
makeStep = Step
