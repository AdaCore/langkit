#! /usr/bin/env python

from __future__ import absolute_import, division, print_function

import argparse

from langkit.compile_context import Verbosity
from langkit.libmanage import LibraryTypes, ManageScript, get_cpu_count


def main():
    m = ManageScript()

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
    build_parser.add_argument(
        '--build-mode', '-b', choices=list(m.BUILD_MODES),
        default='dev',
        help='Selects a preset for build options'
    )
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
    install_parser.add_argument(
        'install-dir',
        help='Installation directory.'
    )
    install_parser.set_defaults(cmd='install-langkit-support')

    args = args_parser.parse_args()

    argv = ['-E', '--build-dir={}'.format(args.build_dir),
            '--verbosity={}'.format(args.verbosity),
            '--library-types={}'.format(args.library_types)]

    argv.append(args.cmd)
    if args.cmd == 'build-langkit-support':
        argv.append('--build-mode={}'.format(args.build_mode))
        if args.gargs:
            argv.append('--gargs={}'.format(args.gargs))
    if args.cmd == 'install-langkit-support':
        argv.append(getattr(args, 'install-dir'))

    m.run(argv)


if __name__ == '__main__':
    main()
