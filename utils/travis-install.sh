#! /usr/bin/env sh

set -v
set -e

if ! [ -d $TOOLS_DIR ]
then
    mkdir -p $TOOLS_DIR
fi
if ! [ -d $INSTALL_DIR ]
then
    mkdir -p $INSTALL_DIR
fi
if ! [ -d $LIB_INSTALL_DIR ]
then
    mkdir -p $LIB_INSTALL_DIR
fi

# Get and install GNAT
if ! [ -d gnat_community_install_script ]
then
    git clone https://github.com/AdaCore/gnat_community_install_script.git
else
    (cd gnat_community_install_script && git pull)
fi
if ! [ -f $INSTALL_DIR/bin/gcc ]
then
    GNAT_INSTALLER=$TOOLS_DIR/gnat-community-2019-20190517-x86_64-linux-bin
    GNAT_INSTALLER_URL="https://community.download.adacore.com/v1/0cd3e2a668332613b522d9612ffa27ef3eb0815b?filename=gnat-community-2019-20190517-x86_64-linux-bin"

    wget -O $GNAT_INSTALLER $GNAT_INSTALLER_URL
    sh gnat_community_install_script/install_package.sh \
        "$GNAT_INSTALLER" "$INSTALL_DIR"
    $INSTALL_DIR/bin/gprinstall --uninstall gnatcoll
fi

# Get gprbuild (to build libgpr)
if [ -d "$TOOLS_DIR/gprbuild" ]
then
    (cd $TOOLS_DIR/gprbuild && git pull)
else
    (cd $TOOLS_DIR && git clone https://github.com/AdaCore/gprbuild)
fi

# Get gnatcoll-core and gnatcoll-bindings
if [ -d "$TOOLS_DIR/gnatcoll-core" ]
then
    (cd $TOOLS_DIR/gnatcoll-core && git pull)
else
    (cd $TOOLS_DIR && git clone https://github.com/AdaCore/gnatcoll-core)
fi
if [ -d "$TOOLS_DIR/gnatcoll-bindings" ]
then
    (cd $TOOLS_DIR/gnatcoll-bindings && git pull)
else
    (cd $TOOLS_DIR && git clone https://github.com/AdaCore/gnatcoll-bindings)
fi

# Log content
pwd
export PATH=$INSTALL_DIR/bin:$PATH
export GPR_PROJECT_PATH=$LIB_INSTALL_DIR/share/gpr
which gcc
gcc -v

# Build libgpr
(
    cd $TOOLS_DIR/gprbuild
    make BUILD=production prefix="$LIB_INSTALL_DIR" libgpr.build
    make BUILD=production prefix="$LIB_INSTALL_DIR" libgpr.install
)

# Build gnatcoll-core
(
    cd $TOOLS_DIR/gnatcoll-core
    make PROCESSORS=0 prefix="$LIB_INSTALL_DIR" ENABLE_SHARED=yes \
       build install
)

# Build gnatcoll-bindings
(
    cd $TOOLS_DIR/gnatcoll-bindings
    for component in iconv gmp
    do
        (
            cd $component
            python setup.py build --reconfigure -j0 \
                --prefix="$LIB_INSTALL_DIR" \
                --library-types=static,relocatable
            python setup.py install
        )
    done
)

# Install Langkit itself and its Python dependencies
pip install .

# Also install flake8 to have complete style checks
pip install flake8

# RA22-015: Install libpythonlang to make it available to dsl_unparse
./scripts/manage.sh make
