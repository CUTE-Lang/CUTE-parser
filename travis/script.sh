#!/usr/bin/env bash
set -ex

case "$BUILD" in
    style)
        source scripts/lint.sh
        stack --no-terminal build --ghc-options -Wall
        ;;

    stack)
        stack --no-terminal $ARGS test --bench --no-run-benchmarks --haddock --no-haddock-deps
        ;;
esac

set +ex
