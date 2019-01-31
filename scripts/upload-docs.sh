#!/bin/sh
set -e

dir=$(mktemp -d dist-docs.XXXXXX)
trap 'rm -fr "$dir"' EXIT

cabal new-haddock --builddir="$dir" --haddock-for-hackage --haddock-option=--hyperlinked-source
cabal upload -d $dir/*-docs.tar.gz
