#!/usr/bin/env bash
set -ex

# Using compiler above sets CC to an invalid value, so unset it
unset CC

# Download and unpack the stack executable
mkdir -p ~/.local/bin

if [ "$TRAVIS_OS_NAME" = "osx" ]
then
    travis_retry curl --insecure -L https://www.stackage.org/stack/osx-x86_64 | tar xz --strip-components=1 --include '*/stack' -C ~/.local/bin
else
    travis_retry curl -L https://www.stackage.org/stack/linux-x86_64 | tar xz --wildcards --strip-components=1 -C ~/.local/bin '*/stack'
fi

set +ex
