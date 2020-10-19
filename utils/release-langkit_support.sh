#! /bin/sh

set -e

RELEASE=21.0.0
SRC_PKG=/tmp/langkit_support-$RELEASE.tar.gz

# Make sure the appropriate Langkit sources are checked out in
# /tmp/langkit-$RELEASE
SRC_DIR=/tmp/langkit-$RELEASE
export PYTHONPATH=$SRC_DIR:$PYTHONPATH

BUILD_DIR=/tmp/langkit_support-$RELEASE
rm -rf $BUILD_DIR

# Generate "langkit_support.gpr", move it to the top directory and import
# sources. Then adjust source/object/library dirs.
$SRC_DIR/scripts/build-langkit_support.py --build-dir=$BUILD_DIR generate
mv $BUILD_DIR/lib/gnat/langkit_support.gpr $BUILD_DIR/
rm -r $BUILD_DIR/lib
mkdir $BUILD_DIR/src
cp -ar $SRC_DIR/langkit/support/*.ad* $BUILD_DIR/src
patch -d $BUILD_DIR -p0 -f -i $SRC_DIR/utils/langkit_support.gpr.patch

# Create the release tarball
tar czf $SRC_PKG -C /tmp langkit_support-$RELEASE
sha512sum $SRC_PKG
