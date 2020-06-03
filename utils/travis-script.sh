#! /usr/bin/env sh

set -v
set -e

export PATH=$INSTALL_DIR/bin:$PATH
export GPR_PROJECT_PATH=$LIB_INSTALL_DIR/share/gpr

# RA22-015: Make libpythonlang available
eval `./scripts/manage.sh setenv`

# Log the toolchain to use
which gcc
which gprbuild
gcc -v
gprbuild -v

# Duplicate output of testsuite in file TESTSUITE_OUT.
#
# TODO: adjust the Travis CI setup to provide a viable OCaml environment and
# enable the corresponding testcases.
./scripts/interactive_testsuite \
    --no-auto-path \
    --disable-ocaml \
    | tee TESTSUITE_OUT

# Exit with an error if there is a FAIL or ERROR line in TESTSUITE_OUT
! grep "^INFO \+\(FAIL\|ERROR\) " TESTSUITE_OUT > /dev/null
