#! /usr/bin/env sh

set -v
set -e

export PATH=$INSTALL_DIR/bin:$PATH
export ADA_PROJECT_PATH=$ADALIB_DIR/share/gpr

# Log the toolchain to use
which gcc
which gprbuild
gcc -v
gprbuild -v

# Duplicate output of testsuite in file TESTSUITE_OUT
./scripts/interactive_testsuite | tee TESTSUITE_OUT

# Exit with an error if there is a FAILED line in
# TESTSUITE_OUT.
if grep "FAILED   " TESTSUITE_OUT; then
    exit 1
else
    exit 0
fi

