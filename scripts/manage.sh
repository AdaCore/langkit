#!/bin/sh

LANGKIT_ROOT=`dirname $0`"/.."

cd "$LANGKIT_ROOT"

make () {

    # We need to clean the build space of the langkit libraries we depend
    # upon before we run langkit again. Else, if we installed a newer version
    # of GNAT since those libraries were built, what will happen is that:
    #
    # 1. Langkit will try loading them.
    # 2. This will cause an uncaught exception trying to load some so from the
    #    compiler, preventing langkit to run.
    # 3. Langkit cannot be used to recompile libpythonlang and liblktlang to
    #    newer versions.
    rm contrib/python/build -rf
    rm contrib/lkt/build -rf

    (
        cd "contrib/python"
        ./manage.py make -P
    )
    (
        cd "contrib/lkt"
        rm build -rf
        ./manage.py make -P
    )
}

setenv () {
    (
        cd "contrib/python"
        ./manage.py setenv
    )
    (
        cd "contrib/lkt"
        ./manage.py setenv
    )
}

if [ "$1" = "make" ]; then
    make
elif [ "$1" = "setenv" ]; then
    setenv
else
    echo "Unknown command : $1"
    exit 1
fi
