#!/usr/bin/env bash

set -ex

if [ -n "$STACK_YAML" ]; then
  stack build --ghc-options -Werror --no-terminal
  stack test --ghc-options -Werror --no-terminal
elif [ -n "$CABAL_NEW_BUILD" ]; then
  GHC_PATH=$(dirname $(stack exec which ghc))
  export PATH=$GHC_PATH:$PATH
  cabal update
  cabal new-build
fi
