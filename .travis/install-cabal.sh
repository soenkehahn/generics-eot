#!/usr/bin/env bash

set -eux

TARBALL=cabal-install-2.0.0.1-x86_64-unknown-linux.tar.gz
wget https://www.haskell.org/cabal/release/cabal-install-2.0.0.1/$TARBALL
tar -xvf $TARBALL -C ~/.local/bin
cabal --version
