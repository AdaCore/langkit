#! /usr/bin/env python

import argparse

from langkit.compile_context import Verbosity
from langkit.libmanage import LibraryTypes, ManageScript, get_cpu_count
from langkit.packaging import Packager


def main():
    m = ManageScript()

    def add_build_mode_arg(parser):
        parser.add_argument(
            '--build-mode', '-b', choices=list(m.BUILD_MODES),
            default='dev',
            help='Selects a preset for build options'
        )

    args_parser = argparse.ArgumentParser(
        description='Helper to build and install Langkit_Support'
    )
    args_parser.add_argument(
        '--build-dir', default='build',
        help='Directory to use for generated source code and binaries. By'
             ' default, use "build" in the current directory.'
    )
    args_parser.add_argument(
        '--library-types', default=LibraryTypes(relocatable=True),
        type=LibraryTypes.parse,
        help='Comma-separated list of library types to build (relocatable,'
             ' static-pic and static). By default, build only shared'
             ' libraries.'
    )
    args_parser.add_argument(
        '--verbosity', '-v', nargs='?',
        type=Verbosity,
        choices=Verbosity.choices(),
        default=Verbosity('info'),
        const=Verbosity('debug'),
        help='Verbosity level'
    )

    subparsers = args_parser.add_subparsers()

    # Generate
    generate_parser = subparsers.add_parser(
        'generate',
        help='Generate build tree and project file for Langkit_Support.'
    )
    generate_parser.set_defaults(cmd='generate-langkit-support')

    # Build
    build_parser = subparsers.add_parser(
        'build',
        help='Build Langkit_Support.'
    )
    build_parser.add_argument(
        '--jobs', '-j', type=int, default=get_cpu_count(),
        help='Number of parallel jobs to spawn in parallel '
             '(default: your number of cpu)'
    )
    add_build_mode_arg(build_parser)
    build_parser.add_argument(
        '--gargs',
        help='Additional arguments to pass to GPRbuild'
    )
    build_parser.set_defaults(cmd='build-langkit-support')

    # Install
    install_parser = subparsers.add_parser(
        'install',
        help='Install Langkit_Support.'
    )
    add_build_mode_arg(install_parser)
    install_parser.add_argument(
        'install-dir',
        help='Installation directory.'
    )
    install_parser.set_defaults(cmd='install-langkit-support')

    # Package dependencies
    pkg_deps_parser = subparsers.add_parser(
        'package-deps',
        help='Bundle all dependencies to complete GNAT Pro'
    )
    pkg_deps_parser.add_argument(
        'package-dir',
        help='Destination directory.'
    )
    Packager.add_prefix_options(pkg_deps_parser)
    Packager.add_platform_options(pkg_deps_parser)
    pkg_deps_parser.set_defaults(cmd='package-deps')

    # Standalone package for dynamic libraries
    pkg_std_dyn_parser = subparsers.add_parser(
        'package-std-dyn',
        help='Bundle all dependencies to create standalone packages'
    )
    pkg_std_dyn_parser.add_argument(
        'package-dir',
        help='Destination directory.'
    )
    Packager.add_prefix_options(pkg_std_dyn_parser)
    Packager.add_platform_options(pkg_std_dyn_parser)
    pkg_std_dyn_parser.set_defaults(cmd='package-std-dyn')

    args = args_parser.parse_args()

    argv = ['-E', '--build-dir={}'.format(args.build_dir),
            '--verbosity={}'.format(args.verbosity),
            '--library-types={}'.format(args.library_types)]

    def add_build_mode():
        argv.append('--build-mode={}'.format(args.build_mode))

    argv.append(args.cmd)
    if args.cmd == 'build-langkit-support':
        add_build_mode()
        if args.gargs:
            argv.append('--gargs={}'.format(args.gargs))
    elif args.cmd == 'install-langkit-support':
        add_build_mode()
        argv.append(getattr(args, 'install-dir'))
    elif args.cmd == 'package-deps':
        p = Packager.from_args(args)
        p.package_deps(getattr(args, 'package-dir'))
        return
    elif args.cmd == 'package-std-dyn':
        p = Packager.from_args(args)
        pkg_dir = getattr(args, 'package-dir')
        p.package_standalone_dyn(pkg_dir)
        p.package_langkit_support_dyn(pkg_dir)
        return

    m.run(argv)


if __name__ == '__main__':
    main()
