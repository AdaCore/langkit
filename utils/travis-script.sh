#! /usr/bin/env sh

set -v
set -e

export PATH=$INSTALL_DIR/bin:$PATH

# Log the toolchain to use
which gcc
which gprbuild
gcc -v
gprbuild -v

# Duplicate output of testsuite in file TESTSUITE_OUT
./scripts/interactive_testsuite \
    --discriminants gnat_community_2018 \
    --no-auto-path \
    | tee TESTSUITE_OUT

# Exit with an error if there is a FAILED line in TESTSUITE_OUT
! grep "FAILED   " TESTSUITE_OUT > /dev/null
