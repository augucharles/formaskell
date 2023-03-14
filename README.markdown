# Hasklean

## Introduction

This formater is a fork of [stylish-haskell](https://github.com/haskell/stylish-haskell).
It has been adapted to strictly apply [Smart-Chain](https://www.smart-chain.fr/) Haskell team style.
It is therefore not as flexible as stylish-haskell and does not aim to be.

## Installation

1) Install Nix by going to the Nix [download page](https://nixos.org/download.html).

2) Enable Flakes by following [this instructions](https://nixos.wiki/wiki/Flakes).

3) Enter the Nix shell:

```shell
nhenin@ubuntu ~/d/hasklean (main)> nix develop
```

4) Build the project

```shell
[nix-shell:~/dev/hasklean]$ cabal build all
```

5) Install for current user:

```shell
[nix-shell:~/dev/hasklean]$ install -D $(cabal list-bin hasklean) $HOME/.local/bin/hasklean
```

6) Open a new terminal or source your .profile file to have the executable in your PATH.

7) Apply formatting on some file:

```shell
nhenin@ubuntu ~/d/hasklean (main)> hasklean -- ./test/Test.hs
```

## Features

- Aligns and sorts `import` statements
- Groups and wraps `{-# LANGUAGE #-}` pragmas, can remove (some) redundant
  pragmas
- Aligns and sorts `module` statement
- Removes trailing whitespace

## Smart-Chain Haskell team style

For now, we only have guidelines for module header, that is `pragma` list, `module` statement and `import` statements.

### Module header

```haskell
{-# LANGUAGE ImportQualifiedPost            #-}
{-# LANGUAGE LambdaCase                     #-}
{-# LANGUAGE OverloadedStrings              #-}
{-# LANGUAGE RecordWildCards                #-}
{-# LANGUAGE TypeApplications               #-}
--                                          ^
module Hasklean.Step.ModuleHeader   --      |
  ( Config (..)                     --      |
  , BreakWhere (..)                 --      |
  , OpenBracket (..)                --      | end of pragma and start of module list are aligned
  , defaultConfig                   --      | with a customizable padding
  , step                            --      |
  ) where                           --      |
--                                          |
--                                          v
import Control.Applicative                  ((<|>))

import Control.Monad as M
--                   ^ alias follows module name when import list is empty

--                    we use ImportQualifiedPost
--                    v
import Data.Aeson qualified
    as Aeson                                ( Value(..) )
--   ^ alias are on new line with
--     module name and alias aligned when
--     import list is not empty

import Plutus.Contract
    ( Contract
    , EmptySchema
    , logInfo
    , ownAddress
    , utxosAt
    )
--  ^ long import list are on new lines on the left with leading comas
```

Note: Rules for grouping `import` statements are on a per-project basis using
POSIX extended regular expressions patterns. Those rules are specified in a configuration file.

## Configuration

The tool is customizable to some extent:

1. You can pass a file to the tool using the `-c/--config` argument
2. Otherwise the default settings will be used.

Use `hasklean --defaults > .hasklean.yaml` to dump a
well-documented default configuration to a file, this way you can get started
quickly.

## Editor integration

### VIM integration

Since it works as a filter it is pretty easy to integrate this with VIM.

You can call

    :%!hasklean

and add a keybinding for it.

Or you can define `formatprg`

    :set formatprg=hasklean

and then use `gq`.
