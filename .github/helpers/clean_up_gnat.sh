#! /bin/sh

# We want to perform the uninstallation in gprinstall's prefix, not in the
# additional installation prefix.
GPR_PROJECT_PATH=
export GPR_PROJECT_PATH

for project in gpr gnatcoll gnatcoll_gmp gnatcoll_iconv langkit_support libadalang
do
    gprinstall --uninstall -P $project || true
done
