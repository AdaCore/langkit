from __future__ import annotations

import argparse
import glob
import os
import subprocess
import sys

from e3.env import Env
from e3.fs import cp, mkdir, sync_tree

from langkit.utils import LibraryType


class BasePackager:
    """
    Common code for packaging.
    """

    def __init__(
        self,
        env: Env,
        library_types: list[LibraryType],
    ):
        """
        :param env: Platform for the libraries to package.
        :param library_types: Set of library types the packages must cover.
        """
        self.env = env
        self.library_types = library_types

        self.with_static = (LibraryType.static_pic in library_types
                            or LibraryType.static in library_types)
        self.with_relocatable = LibraryType.relocatable in library_types

        if self.with_static:
            if (
                self.env.target.os.name == 'linux' and
                self.env.target.cpu.bits == 64
            ):
                self._static_libdir_name = 'lib64'
            else:
                self._static_libdir_name = 'lib'

        if self.with_relocatable:
            self._dyn_libdir_name = (
                'bin' if self.env.target.os.name == 'windows' else 'lib'
            )

        self.is_windows = self.env.build.os.name == 'windows'
        self.dllext = self.env.build.os.dllext

    @property
    def static_libdir_name(self) -> str:
        assert self.with_static
        return self._static_libdir_name

    @property
    def dyn_libdir_name(self) -> str:
        assert self.with_relocatable
        return self._dyn_libdir_name

    def assert_with_relocatable(self) -> None:
        assert LibraryType.relocatable in self.library_types, (
            'Shared libraries support is disabled'
        )

    @staticmethod
    def add_platform_options(parser: argparse.ArgumentParser) -> None:
        """
        Helper to add the --build/--host/--target options to "parser".
        """
        for name in ('build', 'host', 'target'):
            parser.add_argument('--{}'.format(name),
                                help='{} platform'.format(name.capitalize()))

    @staticmethod
    def args_to_env(args: argparse.Namespace) -> Env:
        """
        Create a e3.env.Env instance according to the platform optiong in
        ``args``.
        """
        result = Env()
        result.set_env(args.build, args.host, args.target)
        return result

    def copy_shared_lib(self, pattern: str, dest: str) -> None:
        """
        Copy the shared library (or libraries) matched by "pattern" to the
        "dest" directory.
        """
        self.assert_with_relocatable()
        # On Linux, the name of shared objects files can (but does not need
        # to) be followed by a version number. If both flavors are present,
        # chose the ones with a version number first, as these will be the
        # one the linker will chose.
        if self.env.build.os.name == 'linux' and glob.glob(pattern + '.*'):
            pattern += '.*'
        cp(pattern, dest)

    def std_path(self, prefix: str, lib_subdir: str, libname: str) -> str:
        """
        Return the path to the shared library as installed by gprinstall.

        :param prefix: Prefix given to gprinstall.
        :param lib_subdir: Name of the project subdirectory (generaly, the name
            of the project).
        :param libname: Name of the project.
        """
        self.assert_with_relocatable()
        name = libname + self.dllext
        return (os.path.join(prefix, 'bin', name)
                if self.is_windows else
                os.path.join(prefix,
                             self.dyn_libdir_name,
                             lib_subdir + '.relocatable',
                             name))

    def package_std_dyn(
        self,
        prefix: str,
        lib_subdir: str,
        libname: str,
        package_dir: str,
    ) -> None:
        """
        Copy a dynamic library installed by gprinstall to "package_dir".

        See the std_path method for argument semantics.
        """
        self.assert_with_relocatable()
        self.copy_shared_lib(
            self.std_path(prefix, lib_subdir, libname),
            package_dir
        )


class WheelPackager(BasePackager):
    """
    Helper to build Python wheels.
    """

    def create_python_wheel(
        self,
        python_tag: str,
        plat_name: str,
        wheel_dir: str,
        build_dir: str,
        dyn_deps_dir: str,
        langlib_prefix: str,
        project_name: str,
        lib_name: str | None = None,
        python_interpreter: str | None = None,
    ) -> None:
        """
        Create a Python wheel for a Langkit-generated library.

        :param python_tag: Forwarded to setup.py bdist_wheel's --python-tag
            argument.
        :param plat_name: Forwarded to setup.py bdist_wheel's --plat-name
            argument.
        :param wheel_dir: Destination directory for the wheel.
        :param build_dir: Temporary directory to use in order to build the
            wheel.
        :param dyn_deps_dir: Directory that contains all the dynamic libraries
            to ship in the wheel (i.e. dependencies).
        :param langlib_prefix: Directory in which the Langkit-generated dynamic
            library is installed.
        :param project_name: Name of the GPR project for the Langkit-generated
            library.
        :param lib_name: If provided, name of the dynamic library. If not
            provided, consider it is "lib$project_name".
        :param python_interpreter: If provided, path to the Python interpreter
            to use in order to build the wheel. If left to None, use the
            current interpreter.
        """
        self.assert_with_relocatable()

        lib_name = lib_name or 'lib{}'.format(project_name)
        python_interpreter = python_interpreter or sys.executable

        # Copy Python bindings for the Langkit-generated library and its
        # setup.py script.
        sync_tree(os.path.join(langlib_prefix, 'python'), build_dir,
                  delete=True)

        # Import all required dynamic libraries in the Python package
        package_dir = os.path.join(build_dir, project_name)
        self.package_std_dyn(langlib_prefix, project_name, lib_name,
                             package_dir)
        sync_tree(dyn_deps_dir, package_dir, delete=False)

        # On darwin, make all shared objects look for their dependencies in the
        # same directory.
        if self.env.build.os.name == 'darwin':
            from e3.binarydata.macho import localize_distrib
            localize_distrib(package_dir, [])

        # Finally create the wheel. Make the wheel directory absolute since
        # setup.py is run from the build directory.
        args = [python_interpreter, 'setup.py', 'bdist_wheel',
                '-d', os.path.abspath(wheel_dir)]
        if python_tag:
            args.append(f"--python-tag={python_tag}")
        if plat_name:
            args.append(f"--plat-name={plat_name}")
        subprocess.check_call(args, cwd=build_dir)


class NativeLibPackager(BasePackager):
    """
    Helper to distribute shared/static native libraries.
    """

    def __init__(
        self,
        env: Env,
        library_types: list[LibraryType],
        gnat_prefix: str,
        gmp_prefix: str | None = None,
        libiconv_prefix: str | None = None,
        xmlada_prefix: str | None = None,
        libgpr_prefix: str | None = None,
        gnatcoll_core_prefix: str | None = None,
        gnatcoll_gmp_prefix: str | None = None,
        gnatcoll_iconv_prefix: str | None = None,
        vss_prefix: str | None = None,
        prettier_ada_prefix: str | None = None,
        adasat_prefix: str | None = None,
        langkit_support_prefix: str | None = None,
    ):
        """
        :param env: Platform for the libraries to package.
        :param library_types: Set of library types the packages must cover.
        :param gnat_prefix: Directory in which GNAT is installed.
        :param gmp_prefix: Directory in which GMP is installed. By default, use
            ``gnat prefix``.
        :param libiconv_prefix: Directory in which Libiconv is installed. If
            left to None, consider that there is no need to ship Libiconv.
        :param xmlada_prefix: Directory in which XML/Ada is installed.  By
            default, use ``gnat prefix``.
        :param libgpr_prefix: Directory in which Libgpr is installed.  By
            default, use ``gnat prefix``.
        :param gnatcoll_core_prefix: Directory in which gnatcoll-core is
            installed. By default, use ``gnat prefix``.
        :param gnatcoll_gmp_prefix: Directory in which gnatcoll-bindings(gmp)
            is installed. By default, use ``gnat prefix``.
        :param gnatcoll_iconv_prefix: Directory in which
            gnatcoll-bindings(iconv) is installed. By default, use ``gnat
            prefix``.
        :param vss_prefix: Directory in which VSS is installed.  By default,
            use ``gnat_prefix``.
        :param prettier_ada_prefix: Directory in which Prettier_Ada is
            installed.  By default, use ``gnat_prefix``.
        :param adasat_prefix: Directory in which adasat is installed.  By
            default, use ``gnat_prefix``.
        :param langkit_support_prefix: Directory in which Langkit_Support is
            installed. By default, use ``gnat prefix``.
        """
        super().__init__(env, library_types)
        self.library_types = library_types
        self.gnat_prefix = gnat_prefix
        self.gmp_prefix = gmp_prefix or gnat_prefix
        self.libiconv_prefix = libiconv_prefix
        self.xmlada_prefix = xmlada_prefix or gnat_prefix
        self.libgpr_prefix = libgpr_prefix or gnat_prefix
        self.gnatcoll_core_prefix = gnatcoll_core_prefix or gnat_prefix
        self.gnatcoll_gmp_prefix = gnatcoll_gmp_prefix or gnat_prefix
        self.gnatcoll_iconv_prefix = gnatcoll_iconv_prefix or gnat_prefix
        self.vss_prefix = vss_prefix or gnat_prefix
        self.prettier_ada_prefix = prettier_ada_prefix or gnat_prefix
        self.adasat_prefix = adasat_prefix or gnat_prefix
        self.langkit_support_prefix = langkit_support_prefix or gnat_prefix

    @staticmethod
    def add_prefix_options(parser: argparse.ArgumentParser) -> None:
        """
        Helper to add all the "--with-XXX" options to "parser".

        These options are used to convey installation prefixes for
        dependencies.
        """
        for name in ('gnat', 'gmp', 'libiconv', 'xmlada', 'libgpr',
                     'gnatcoll-core', 'gnatcoll-gmp', 'gnatcoll-iconv',
                     'vss', 'prettier-ada', 'adasat', 'langkit-support'):
            parser.add_argument(
                '--with-{}'.format(name),
                help='Installation directory for {}'.format(name)
            )

    @classmethod
    def from_args(cls, args: argparse.Namespace) -> NativeLibPackager:
        """
        Instantiate Packager from command-line arguments.
        """
        return cls(
            cls.args_to_env(args),
            args.library_types,
            args.with_gnat,
            args.with_gmp,
            args.with_libiconv,
            args.with_xmlada,
            args.with_libgpr,
            args.with_gnatcoll_core,
            args.with_gnatcoll_gmp,
            args.with_gnatcoll_iconv,
            args.with_vss,
            args.with_prettier_ada,
            args.with_adasat,
            args.with_langkit_support
        )

    def package_deps(self, package_dir: str) -> None:
        """
        Copy all libraries that are not part of GNAT Pro to the package
        directory.

        Once this is done, this package + GNAT Pro can be used in order to
        build Ada projects that depend on Langkit-generated libraries.

        :param package_dir: Name of the directory where the package should be
            created.
        """
        # Destination directory for copies of static libs. Make sure it exists.
        if self.with_static:
            static_libdir = os.path.join(package_dir, self.static_libdir_name)
            mkdir(static_libdir)

        # Likewise for the destination directory for copies of dynamic libs
        if self.with_relocatable:
            dyn_libdir = os.path.join(package_dir, self.dyn_libdir_name)
            mkdir(dyn_libdir)

        def copy_in(filename: str, dirname: str) -> None:
            """Copy the "filename" to the "dirname" directory."""
            cp(filename, os.path.join(dirname, os.path.basename(filename)))

        # Ship non-GNAT libraries. Copy all files that gprinstall created:
        # shared libs, static libs, manifests, sources, etc.
        for prefix, name in [
            (self.gnatcoll_gmp_prefix, 'gnatcoll_gmp'),
            (self.gnatcoll_iconv_prefix, 'gnatcoll_iconv'),
            (self.vss_prefix, 'vss'),
            (self.prettier_ada_prefix, 'prettier_ada'),
        ]:
            # In all of the following directories, look for files/directories
            # that matches "*$name*" and copy them in $package_dir, preserving
            # the directory hierarchy.
            for d in ('bin', 'include', 'lib',
                      os.path.join('share', 'gpr'),
                      os.path.join('share', 'gpr', 'manifests')):
                to_copy = glob.glob(os.path.join(prefix, d, f'*{name}*'))
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
        if self.with_static:
            lib_files = [os.path.join(self.gmp_prefix, 'lib', 'libgmp.a')]
            if self.libiconv_prefix:
                lib_files.append(os.path.join(
                    self.libiconv_prefix, 'lib', 'libiconv.a'
                ))
            for f in lib_files:
                copy_in(f, static_libdir)

        # Ship libiconv's shared lib, as needed by the shared
        # libgnatcoll_iconv.
        if self.with_relocatable and self.libiconv_prefix:
            for item in glob.glob(os.path.join(
                self.libiconv_prefix,
                self.dyn_libdir_name,
                'libiconv*' + self.dllext + '*'
            )):
                copy_in(item, dyn_libdir)

        # Ship AdaSAT as well. We can simply copy the whole package
        sync_tree(self.adasat_prefix, package_dir, delete=False)

    def xmlada_path(self, name: str, dirname: str | None = None) -> str:
        """
        Special case for XML/Ada libraries.
        """
        self.assert_with_relocatable()
        libname = 'libxmlada_' + name + self.dllext
        return (
            os.path.join(self.xmlada_prefix, 'bin', libname)
            if self.is_windows else
            os.path.join(self.xmlada_prefix, self.dyn_libdir_name,
                         'xmlada',
                         'xmlada_{}.relocatable'.format(dirname or name),
                         libname)
        )

    def vss_path(self, lib_radix: str, project_name: str) -> str:
        """
        Special case for VSS libraries.
        """
        self.assert_with_relocatable()
        libname = "lib" + lib_radix + self.dllext
        return (
            os.path.join(self.vss_prefix, "bin", libname)
            if self.is_windows else
            os.path.join(
                self.vss_prefix,
                self.dyn_libdir_name,
                "vss",
                "relocatable",
                project_name,
                libname,
            )
        )

    def package_standalone_dyn(self, package_dir: str) -> None:
        """
        Copy the complete closure of dynamic libraries dependencies for
        Langkit-generated libraries to the given directory.

        This closure includes even the GNAT runtime, but excludes system
        libraries, such as the libc.

        This is useful to create completely standalone Python wheels.
        """
        self.assert_with_relocatable()

        # Locate the native runtime's "adalib" directory using gnatls
        gnatls_output = subprocess.check_output(
            [os.path.join(self.gnat_prefix, 'bin', 'gnatls'),
             '-a', 'system.o'],
            encoding='ascii')
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
        # Compute paths of former gnatcoll library.
        # TODO: remove after 02/2024 stable bump support for former gnatcoll
        # packaging scheme. See eng/toolchain/gnatcoll-core#25.
        gnatcoll_core_libs = [
            self.std_path(self.gnatcoll_core_prefix, "gnatcoll", "libgnatcoll")
        ]
        if not os.path.exists(gnatcoll_core_libs[0]):
            # The directory does not exist so try the new scheme that contains
            # two libraries.
            gnatcoll_core_libs = [
                self.std_path(
                    self.gnatcoll_core_prefix, "gnatcoll_core",
                    "libgnatcoll_core"
                ),
                self.std_path(
                    self.gnatcoll_core_prefix,
                    "gnatcoll_projects",
                    "libgnatcoll_projects",
                ),
            ]
            # New split of gnatcoll_core into two libraries introduced in
            # July 2024.
            gnatcoll_minimal_path = self.std_path(
                self.gnatcoll_core_prefix,
                "gnatcoll_minimal",
                "libgnatcoll_minimal",
            )
            if os.path.exists(gnatcoll_minimal_path):
                gnatcoll_core_libs.append(gnatcoll_minimal_path)

        gnatcoll_bindings_libs = [
            self.std_path(self.gnatcoll_iconv_prefix, 'gnatcoll_iconv',
                          'libgnatcoll_iconv'),
            self.std_path(self.gnatcoll_gmp_prefix, 'gnatcoll_gmp',
                          'libgnatcoll_gmp')]

        # VSS
        vss_libs = [
            self.vss_path("vss", "vss_text"),
            self.vss_path("vss-gnat", "vss_gnat"),
            self.vss_path("vss-json", "vss_json"),
            self.vss_path("vss-regexp", "vss_regexp"),
            self.vss_path("vss-xml-templates", "vss_xml_templates"),
            self.vss_path("vss-xml-xmlada", "vss_xml_xmlada"),
            self.vss_path("vss-xml", "vss_xml"),
        ]

        # Prettier_Ada
        prettier_ada_libs = [
            self.std_path(
                self.prettier_ada_prefix,
                os.path.join('prettier_ada', 'prettier_ada'),
                'libprettier_ada',
            )
        ]

        # AdaSAT
        adasat_lib = [self.std_path(self.adasat_prefix, 'adasat', 'libadasat')]

        # Finally, do the copy
        for libpath in (gnat_runtime_libs +
                        xmlada_libs +
                        gpr_libs +
                        libiconv_libs +
                        gnatcoll_core_libs +
                        gnatcoll_bindings_libs +
                        vss_libs +
                        prettier_ada_libs +
                        adasat_lib):
            self.copy_shared_lib(libpath, package_dir)

    def package_langkit_support_dyn(self, package_dir: str) -> None:
        """
        Copy the Langkit_Support dynamic library to "package_dir".
        """
        self.assert_with_relocatable()
        self.package_std_dyn(self.langkit_support_prefix, 'langkit_support',
                             'liblangkit_support', package_dir)
