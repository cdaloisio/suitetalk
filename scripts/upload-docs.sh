#!/bin/sh
set -e

dir=$(mktemp -d dist-docs.XXXXXX)
trap 'rm -r "$dir"' EXIT

cabal new-haddock --builddir="$dir" --for-hackage --haddock-option=--hyperlinked-source
cabal upload -d $dir/*-docs.tar.gz
