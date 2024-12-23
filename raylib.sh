#!/usr/bin/env sh

# Use this to compile the adapters. Without these, the system won't load into Lisp.

set -e

DIR=vendored/claw-raylib/lib
ARCH=x86_64-pc-linux-gnu

gcc -O3 -fPIC -shared -o ${DIR}/libraylib-adapter.so ${DIR}/libraylib-adapter.${ARCH}.c
gcc -O3 -fPIC -shared -o ${DIR}/librlgl-adapter.so ${DIR}/librlgl-adapter.${ARCH}.c
