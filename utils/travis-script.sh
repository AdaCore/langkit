#! /usr/bin/env sh

set -v
set -e

export PATH=$INSTALL_DIR/bin:$PATH
export GPR_PROJECT_PATH=$LIB_INSTALL_DIR/share/gpr

# RA22-015: Make libpythonlang & liblktlang available
eval `./manage.py setenv`

# Log the toolchain to use
which gcc
which gprbuild
gcc -v
gprbuild -v

# Type check Langkit using mypy
mypy --config-file=mypy.ini

# Exit with an error if there is a test failure/error.
#
# TODO: adjust the Travis CI setup to provide a viable OCaml environment and
# enable the corresponding testcases.
./manage.py test \
    --no-auto-path \
    --disable-ocaml \
    --failure-exit-code=1
