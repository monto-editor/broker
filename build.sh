#!/bin/sh

cabal update
cabal sandbox init
cabal install --dependencies-only
cabal build
