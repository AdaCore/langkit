from __future__ import absolute_import, division, print_function

import glob
import os
import subprocess

from e3.env import Env
from e3.fs import cp, mkdir, sync_tree


class Packager(object):
    """
    Helper to distribute Langkit-generated libraries.
    """

    def __init__(
        self, library_types,
        gnat_prefix,
        gmp_prefix=None,
        libiconv_prefix=None,
        xmlada_prefix=None,
        libgpr_prefix=None,
        gnatcoll_core_prefix=None,
        gnatcoll_gmp_prefix=None,
        gnatcoll_iconv_prefix=None,
        langkit_support_prefix=None,
    ):
        """
        :param langkit.libmanage.LibraryTypes library_types: Set of library
            types the packages must cover.
        :param str gnat_prefix: Directory in which GNAT is installed.
        :param None|str gmp_prefix: Directory in which GMP is installed. By
            default, use ``gnat prefix``.
        :param None|str libiconv_prefix: Directory in which Libiconv is
            installed. If left to None, consider that there is no need to ship
            Libiconv.
        :param None|str xmlada_prefix: Directory in which XML/Ada is installed.
            By default, use ``gnat prefix``.
        :param None|str libgpr_prefix: Directory in which Libgpr is installed.
            By default, use ``gnat prefix``.
        :param None|str gnatcoll_core_prefix: Directory in which gnatcoll-core
            is installed. By default, use ``gnat prefix``.
        :param None|str gnatcoll_gmp_prefix: Directory in which
            gnatcoll-bindings(gmp) is installed. By default, use ``gnat
            prefix``.
        :param None|str gnatcoll_iconv_prefix: Directory in which
            gnatcoll-bindings(iconv) is installed. By default, use ``gnat
            prefix``.
        :param None|str langkit_support_prefix: Directory in which
            Langkit_Support is installed. By default, use ``gnat prefix``.
        """
        self.library_types = library_types
        self.gnat_prefix = gnat_prefix
        self.gmp_prefix = gmp_prefix or gnat_prefix
        self.libiconv_prefix = libiconv_prefix
        self.xmlada_prefix = xmlada_prefix or gnat_prefix
        self.libgpr_prefix = libgpr_prefix or gnat_prefix
        self.gnatcoll_core_prefix = gnatcoll_core_prefix or gnat_prefix
        self.gnatcoll_gmp_prefix = gnatcoll_gmp_prefix or gnat_prefix
        self.gnatcoll_iconv_prefix = gnatcoll_iconv_prefix or gnat_prefix
        self.langkit_support_prefix = langkit_support_prefix or gnat_prefix

        self.env = Env()
        self.static_libdir_name = 'lib'
        if (
            self.env.target.os.name == 'linux' and
            self.env.target.cpu.bits == 64
        ):
            self.static_libdir_name = 'lib64'
        self.dyn_libdir_name = (
            'bin' if self.env.target.os.name == 'windows' else 'lib'
        )
        self.is_windows = self.env.build.os.name == 'windows'
        self.dllext = self.env.build.os.dllext

    @staticmethod
    def add_prefix_options(parser):
        """
        Helper to add all the "--with-XXX" options to "parser".

        These options are used to convey installation prefixes for
        dependencies.
        """
        for name in ('gnat', 'gmp', 'libiconv', 'xmlada', 'libgpr',
                     'gnatcoll-core', 'gnatcoll-gmp', 'gnatcoll-iconv',
                     'langkit-support'):
            parser.add_argument(
                '--with-{}'.format(name),
                help='Installation directory for {}'.format(name)
            )

    @classmethod
    def from_args(cls, args):
        """
        Instanciate Packager from command-line arguments.
        """
        return cls(
            args.library_types,
            args.with_gnat,
            args.with_gmp,
            args.with_libiconv,
            args.with_xmlada,
            args.with_libgpr,
            args.with_gnatcoll_core,
            args.with_gnatcoll_gmp,
            args.with_gnatcoll_iconv,
            args.with_langkit_support
        )

    def package_deps(self, package_dir):
        """
        Copy all libraries that are not part of GNAT Pro to the package
        directory.

        Once this is done, this package + GNAT Pro can be used in order to
        build Ada projects that depend on Langkit-generated libraries.

        :param str package_dir: Name of the directory where the package should
            be created.
        """
        # Destination directory for copies of static libs. Make sure it exists.
        static_libdir = os.path.join(package_dir, self.static_libdir_name)
        mkdir(static_libdir)

        # Likewise for the destination directory for copies of dynamic libs
        dyn_libdir = os.path.join(package_dir, self.dyn_libdir_name)
        mkdir(dyn_libdir)

        def copy_in(filename, dirname):
            """Copy the "filename" to the "dirname" directory."""
            cp(filename, os.path.join(dirname, os.path.basename(filename)))

        # Ship gnatcoll-iconv and gnatcoll-gmp. Copy all files that gprinstall
        # created: shared libs, static libs, manifests, sources, etc.
        for prefix, name in [
            (self.gnatcoll_gmp_prefix, 'gmp'),
            (self.gnatcoll_iconv_prefix, 'iconv'),
        ]:
            # In all of the following directories, look for files/directories
            # that matches "*gnatcoll_$name*" and copy them in $package_dir,
            # preserving the directory hierarchy.
            for d in ('bin', 'include', 'lib',
                      os.path.join('share', 'gpr'),
                      os.path.join('share', 'gpr', 'manifests')):
                to_copy = glob.glob(os.path.join(
                    prefix, d, '*gnatcoll_{}*'.format(name)
                ))
                for item in to_copy:
                    rel_item = os.path.relpath(item, prefix)
                    sync_tree(item,
                              os.path.join(package_dir, rel_item),
                              delete=False)

        # TODO??? For some reason, gnatcoll_gmp's project file tells the linker
        # to always put "-lgmp" although it's not needed when linking with
        # libgnatcoll_gmp.so (as it contains libgmp.a already). As a result,
        # linking programs with Libadalang can fail because of missing gmp
        # although it's already available. Investigation happens under
        # R613-014. To workaround this, just provide the static library.
        #
        # Likewise for gnatcoll_iconv/libiconv.a.
        #
        # On Linux 64-bit, copy these static libraries to lib64 so that they
        # take precedence over shared libs that would also be in lib64
        # directories.
        #
        # So ship gmp and libiconv.
        lib_files = [os.path.join(self.gmp_prefix, 'lib', 'libgmp.a')]
        if self.libiconv_prefix:
            lib_files.append(os.path.join(
                self.libiconv_prefix, 'lib', 'libiconv.a'
            ))
        for f in lib_files:
            copy_in(f, static_libdir)

        # Ship libiconv's shared lib, as needed by the shared
        # libgnatcoll_iconv.
        if self.libiconv_prefix:
            for item in glob.glob(os.path.join(
                self.libiconv_prefix,
                self.dyn_libdir_name,
                'libiconv*' + self.dllext + '*'
            )):
                copy_in(item, dyn_libdir)

    def std_path(self, prefix, lib_subdir, libname):
        """
        Return the path to the shared library as installed by gprinstall.

        :param str prefix: Prefix given to gprinstall.
        :param str lib_subdir: Name of the project subdirectory (generaly,
            the name of the project).
        :param str libname: Name of the project.
        """
        name = libname + self.dllext
        return (os.path.join(prefix, 'bin', name)
                if self.is_windows else
                os.path.join(prefix,
                             self.dyn_libdir_name,
                             lib_subdir + '.relocatable',
                             name))

    def xmlada_path(self, name, dirname=None):
        """
        Special case for XML/Ada libraries.
        """
        libname = 'libxmlada_' + name + self.dllext
        return (
            os.path.join(self.xmlada_prefix, 'bin', libname)
            if self.is_windows else
            os.path.join(self.xmlada_prefix, self.dyn_libdir_name,
                         'xmlada',
                         'xmlada_{}.relocatable'.format(dirname or name),
                         libname)
        )

    def copy_shared_lib(self, pattern, dest):
        """
        Copy the shaed library (or libraries) matched by "pattern" to the
        "dest" directory.
        """
        # On Linux, the name of shared objects files can (but does not need
        # to) be followed by a version number. If both flavors are present,
        # chose the ones with a version number first, as these will be the
        # one the linker will chose.
        if self.env.build.os.name == 'linux' and glob.glob(pattern + '.*'):
            pattern += '.*'
        cp(pattern, dest)

    def package_standalone_dyn(self, package_dir):
        """
        Copy the complete closure of dynamic libraries dependencies for
        Langkit-generated libraries to the given directory.

        This closure includes even the GNAT runtime, but excludes system
        libraries, such as the libc.

        This is useful to create completely standalone Python wheels.
        """

        # Locate the native runtime's "adalib" directory using gnatls
        gnatls_output = subprocess.check_output(
            [os.path.join(self.gnat_prefix, 'bin', 'gnatls'),
             '-a', 'system.o'])
        adalib = os.path.dirname(gnatls_output.splitlines()[0])

        # Compute the list of all dynamic libraries to copy

        # GNAT runtime
        gnat_runtime_libs = [
            os.path.join(adalib, 'libgnat-*' + self.dllext),
            os.path.join(adalib, 'libgnarl-*' + self.dllext),
            os.path.join(self.gnat_prefix, self.dyn_libdir_name + '*',
                         'libgcc_s*{}*'.format(self.dllext))]

        # XML/Ada
        xmlada_libs = [
            self.xmlada_path('dom'),
            self.xmlada_path('input_sources', 'input'),
            self.xmlada_path('sax'),
            self.xmlada_path('schema'),
            self.xmlada_path('unicode')]

        # Libgpr
        gpr_libs = [os.path.join(self.libgpr_prefix, 'lib', 'gpr',
                                 'relocatable', 'gpr', 'libgpr' + self.dllext)]

        # Libiconv, if provided
        if not self.libiconv_prefix:
            libiconv_libs = []
        elif self.is_windows:
            libiconv_libs = [os.path.join(self.libiconv_prefix,
                                          'bin', 'libiconv-*' + self.dllext)]
        else:
            libiconv_libs = [os.path.join(self.libiconv_prefix,
                                          'lib*', 'libiconv' + self.dllext)]

        # GNATcoll (core and bindings)
        gnatcoll_core_libs = [self.std_path(self.gnatcoll_core_prefix,
                                            'gnatcoll', 'libgnatcoll')]
        gnatcoll_bindings_libs = [
            self.std_path(self.gnatcoll_iconv_prefix, 'gnatcoll_iconv',
                          'libgnatcoll_iconv'),
            self.std_path(self.gnatcoll_gmp_prefix, 'gnatcoll_gmp',
                          'libgnatcoll_gmp')]

        # Finally, do the copy
        for libpath in (gnat_runtime_libs +
                        xmlada_libs +
                        gpr_libs +
                        libiconv_libs +
                        gnatcoll_core_libs +
                        gnatcoll_bindings_libs):
            self.copy_shared_lib(libpath, package_dir)

    def package_std_dyn(self, prefix, lib_subdir, libname, package_dir):
        """
        Copy a dynamic library installed by gprinstall to "package_dir".

        See the std_path method for argument semantics.
        """
        self.copy_shared_lib(
            self.std_path(prefix, lib_subdir, libname),
            package_dir
        )

    def package_langkit_support_dyn(self, package_dir):
        """
        Copy the Langkit_Support dynamic library to "package_dir".
        """
        self.package_std_dyn(self.langkit_support_prefix, 'langkit_support',
                             'liblangkit_support', package_dir)
