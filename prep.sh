#!/usr/bin/env sh

set -e

PROJECT_ROOT=$(pwd)

echo "--- RAYLIB ---"
cd vendored/raylib/src/
make
cd "${PROJECT_ROOT}"

echo "--- SHIM ---"
cd raylib/
gcc -O3 -fPIC -shared -o libshim.so shim.c
cd "${PROJECT_ROOT}"

echo "--- COPYING ---"
mkdir -p lib
cd lib/
mv ../vendored/raylib/src/libraylib.so.5.5.0 libraylib.so
mv ../raylib/libshim.so .
ln -s libraylib.so libraylib.so.550
cd "${PROJECT_ROOT}"

echo "--- LINKS ---"
ln -s raylib/shim.h shim.h
ln -s vendored/raylib/src/raylib.h raylib.h

echo "--- DONE ---"
echo "Now run build.lisp with either SBCL or ECL."
