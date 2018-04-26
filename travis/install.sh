#!/usr/bin/env bash
set -ex

stack --no-terminal --install-ghc $ARGS install alex happy

case "$BUILD" in
    style)
        stack --no-terminal --install-ghc $ARGS install hlint
        ;;
    stack)
        stack --no-terminal --install-ghc $ARGS test --bench --only-dependencies
        ;;
esac

set +ex
