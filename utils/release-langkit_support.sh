#! /bin/sh

set -e

RELEASE=23.0.0
SRC_PKG=/tmp/langkit_support-$RELEASE.tar.gz

# Make sure the appropriate Langkit sources are checked out in
# /tmp/langkit-$RELEASE
SRC_DIR=/tmp/langkit-$RELEASE
export PYTHONPATH=$SRC_DIR:$PYTHONPATH

BUILD_DIR=/tmp/langkit_support-$RELEASE
rm -rf $BUILD_DIR

# Generate "langkit_support.gpr", move it to the top directory and import
# sources. Then adjust source/object/library dirs.
cp -ar $SRC_DIR/langkit/support/ $BUILD_DIR
rm -rf $BUILD_DIR/obj
rm -rf $BUILD_DIR/lib

# Create the release tarball
tar czf $SRC_PKG -C /tmp langkit_support-$RELEASE
sha512sum $SRC_PKG
