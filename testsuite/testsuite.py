#! /usr/bin/env python

"""
Usage::

    testsuite.py [OPTIONS]

Run the Langkit testsuite.
"""

import glob
import os
import shutil
import subprocess
import sys

from e3.testsuite import Testsuite

import drivers.langkit_support_driver
import drivers.lkt_resolve_driver
import drivers.python_driver


def add_to_path(env_var, directory):
    """
    Add the given directory to the `env_var` path.
    """
    path = os.environ.get(env_var)
    path = ('{}{}{}'.format(directory, os.path.pathsep, path)
            if path else
            env_var)
    os.environ[env_var] = path


class LangkitTestsuite(Testsuite):
    tests_subdir = 'tests'
    test_driver_map = {
        'python': drivers.python_driver.PythonDriver,
        'langkit_support': drivers.langkit_support_driver.LangkitSupportDriver,
        'lkt': drivers.lkt_resolve_driver.LktResolveDriver,
    }

    def add_options(self, parser):
        parser.add_argument(
            '--with-python',
            help='If provided, use as the Python interpreter in testcases.'
        )

        parser.add_argument(
            '--coverage', '-C', action='store_true',
            help='Enable computation of code coverage for Langkit and'
                 ' Langkit_Support. This requires coverage.py and'
                 ' GNATcoverage.'
        )

        parser.add_argument(
            '--valgrind', action='store_true',
            help='Run tests with Valgrind to check memory issues.'
        )

        parser.add_argument(
            '--no-auto-path', action='store_true',
            help='Do not automatically add Langkit to PYTHONPATH. This is'
                 ' useful to test that Langkit was properly installed in the'
                 ' environment that runs the testsuite.'
        )

        parser.add_argument(
            '--disable-ocaml', action='store_true',
            help='Disable testcases that require OCaml (they are enabled by'
                 ' default).'
        )

        parser.add_argument(
            '--restricted-env', action='store_true',
            help='Skip testcases that cannot run in a restricted environment'
                 ' (need for non-standard Python packages).'
        )

        #
        # Convenience options for developpers
        #

        parser.add_argument(
            '--disable-tear-up-builds', '-B', action='store_true',
            help='Disable the automatic build of Langkit_Support during the'
                 ' testsuite tear_up step. This is used to speed up successive'
                 ' testsuite runs during development.'
        )
        parser.add_argument(
            '--pretty-print', action='store_true',
            help='Pretty-print generated source code.'
        )

        # Tests update
        parser.add_argument(
            '--rewrite', '-r', action='store_true',
            help='Rewrite test baselines according to current output.'
        )

    def set_up(self):
        super().set_up()
        self.env.rewrite_baselines = self.env.options.rewrite
        self.env.control_condition_env = {
            'restricted_env': self.env.options.restricted_env,
        }

        if self.env.options.coverage:
            # Create a directory that we'll use to:
            #
            #   1) collect coverage data for each testcase;
            #   2) generate the HTML report.
            self.env.coverage_dir = os.path.join(self.output_dir, 'coverage')
            self.env.langkit_support_coverage_dir = os.path.join(
                self.env.coverage_dir, 'langkit_support'
            )
            os.mkdir(self.env.coverage_dir)
            os.mkdir(self.env.langkit_support_coverage_dir)

        def report(p, pname):
            if p.returncode != 0:
                raise RuntimeError(
                    '{} build failed (GPRbuild returned {})\n{}'.format(
                        pname, p.returncode, p.stdout
                    )
                )

        # Build Langkit_Support so that each testcase does not try to build it
        # in parallel. To achieve this, we build+install it with all library
        # kinds, and then update the environment so that testcases can assume
        # it is installed.
        if not self.env.options.disable_tear_up_builds:
            self.env.langkit_support_project_file = os.path.join(
                self.root_dir,
                '..', 'support', 'langkit_support.gpr'
            )
            install_prefix = os.path.join(self.working_dir, 'install')
            if os.path.exists(install_prefix):
                shutil.rmtree(install_prefix)

            gargs = ['-p', '-P', self.env.langkit_support_project_file]
            cargs = ['-cargs', '-O0', '-g', '-gnatwae']
            if self.env.options.coverage:
                gargs.append('--subdirs=gnatcov')
                cargs.extend(['-fdump-scos', '-fpreserve-control-flow'])

            for build in ('static', 'relocatable'):
                p = subprocess.run(
                    ['gprbuild'] + gargs +
                    ['-XLIBRARY_TYPE={}'.format(build)] + cargs,
                    stdout=subprocess.PIPE,
                    stderr=subprocess.STDOUT,
                    encoding='utf-8',
                )
                report(p, 'Langkit support - build {}'.format(build))

                p = subprocess.run(
                    ['gprinstall',
                     '-P', self.env.langkit_support_project_file,
                     '-p', '--sources-subdir=include/langkit_support',
                     '-XLIBRARY_TYPE={}'.format(build),
                     '--prefix={}'.format(install_prefix),
                     '--build-var=LIBRARY_TYPE',
                     '--build-name={}'.format(build)],
                    stdout=subprocess.PIPE,
                    stderr=subprocess.STDOUT,
                    encoding='utf-8',
                )
                report(p, 'Langkit support - install {}'.format(build))

            # Make the installed library available to all testcases
            add_to_path('PATH', os.path.join(install_prefix, 'bin'))
            add_to_path('LD_LIBRARY_PATH', os.path.join(install_prefix, 'lib'))
            add_to_path('GPR_PROJECT_PATH',
                        os.path.join(install_prefix, 'share', 'gpr'))

        # Make the "python_support" directory available to LKT import
        # statements.
        os.environ['LKT_PATH'] = os.path.join(self.root_dir, 'python_support')

    def tear_down(self):
        if self.env.options.coverage:
            # Consolidate coverage data for each testcase and generate both a
            # sumary textual report on the standard output and a detailed HTML
            # report.
            langkit_cov_files = glob.glob(os.path.join(self.env.coverage_dir,
                                                       '*.coverage'))

            lksp_cov_files = glob.glob(os.path.join(self.env.coverage_dir,
                                                    '*.trace'))

            if langkit_cov_files:
                rcfile = os.path.join(self.root_dir, '..', 'coverage.ini')
                for argv in (
                    ['combine'] + langkit_cov_files,
                    ['report'],
                    ['html', '-d.', '--title=Langkit coverage report'],
                ):
                    if argv[0] != 'combine':
                        argv.append('--rcfile=' + rcfile)
                    subprocess.check_call(['coverage'] + argv,
                                          cwd=self.env.coverage_dir)

                html_index = os.path.join(self.env.coverage_dir, 'index.html')
                assert os.path.exists(html_index)
                print('Detailed HTML coverage report for Langkit available at:'
                      ' {}'.format(html_index))
            else:
                print('No test exercised Langkit: no coverage report to'
                      ' produce')

            if lksp_cov_files:
                subprocess.check_call([
                    'gnatcov', 'coverage',
                    '-P', self.env.langkit_support_project_file,
                    '--level=stmt+decision',
                    '--annotate=dhtml',
                    '--output-dir', self.env.langkit_support_coverage_dir,
                    '--subdirs=gnatcov',
                ] + lksp_cov_files)
                print('HTML coverage report for Langkit_Support: {}'.format(
                    os.path.join(self.env.langkit_support_coverage_dir,
                                 'index.html')
                ))
            else:
                print('No test exercised Langkit_Support: no coverage report'
                      ' to produce')

        super().tear_down()


if __name__ == '__main__':
    sys.exit(LangkitTestsuite().testsuite_main())
