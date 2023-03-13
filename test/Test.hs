{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <&>" #-}
{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant <$>" #-}

module Module.Name (functionC, functionG, functionB, ImportA, functionA, ImportI, functionF
    , functions, ModuleA(..), TypeR) where

import Group.A.Module.A (functionC, functionA)
import Group.A.Module.D (functionB)
import Group.A.Module.B (functionD)
import Group.A.Module.C
import Module.With.Empty.Imports.List ()
import qualified Module.Pre.Qualified as MPQ
import Module.As.But.Not.Qualified as MABNQ
import Module.As.With.Some.Imports as MAWSI (functionZ, TypeZ, DataZ)
import Group.B.Module.C
import Module.With.Lot.Of.Imports.And.Qualified qualified as MWLOIAQ
    ( ImportD
    , ImportA
    , ImportF
    , ImportC
    , ImportB(ImportBA)
    , ImportI(..)
    , ImportL(ImportLA)
    , ImportE
    , ImportH
    , ImportJ(ImportJA)
    , ImportK(ImportKC, ImportKA, ImportKB, ImportKD)
    , ImportG(ImportGA, functionG)
    , ImportN(ImportNA, ImportNB)
    , ImportM(ImportMA)
    , ImportO
    , ImportP
    , functionF
    , functionE
    )
import Group.B.Module.A
import Module.Hiding.Few.Things hiding ( functionA, functionC )
import Module.Hiding.Lot.Of.Things hiding ( functionA, functionC, functionB, functionE, TypeA, TypeB, ImportM, ImportP )
import qualified Module.Qualified.Hiding.Few.Things as MQHFT hiding ( functionA, functionC )
import qualified Module.Qualified.Hiding.Lot.Of.Things as MQHLOT hiding ( functionA, functionC, functionB, functionE, TypeA, TypeB, ImportM, ImportP )
import Group.B.Module.B
import Module.As.Hiding.Few.Things as MAHFT hiding ( functionA, functionC )
import Module.As.Hiding.Lot.Of.Things as MAHLOT hiding ( functionA, functionC, functionB, functionE, TypeA, TypeB, ImportM, ImportP )
import Module.With.Very.Very.Very.Very.Very.Very.Very.Very.Very.Very.Very.Very.Very.Very.Very.Very.Very.Very.Very.Very.Long.Name (TypeA, TypeB, TypeD, TypeF)
