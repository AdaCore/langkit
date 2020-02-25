#!/bin/sh

LANGKIT_ROOT=`dirname $0`"/.."

cd "$LANGKIT_ROOT"

make () {
    (
        cd "contrib/python"
        ./manage.py make -P
    )
    (
        cd "contrib/lkt"
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
