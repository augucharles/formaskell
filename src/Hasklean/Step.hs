--------------------------------------------------------------------------------
module Hasklean.Step
    ( Lines
    , Step(..)
    , makeStep
    ) where


--------------------------------------------------------------------------------
import Hasklean.Module                                 ( Lines, Module )

--------------------------------------------------------------------------------
data Step = Step
    { stepName   :: String
    , stepFilter :: Lines -> Module -> Lines
    }

--------------------------------------------------------------------------------
makeStep :: String -> (Lines -> Module -> Lines) -> Step
makeStep = Step
