from __future__ import absolute_import, division, print_function

import glob
import os
import subprocess

# pyflakes off
with_gnatpython = False
if not os.environ.get('WITHOUT_GNATPYTHON'):
    try:
        from gnatpython.ex import Run, PIPE
        from gnatpython.testsuite import Testsuite as BaseTestsuite
    except ImportError:
        pass
    else:
        with_gnatpython = True
if not with_gnatpython:
    from testsuite_support.polyfill import BaseTestsuite, Run, PIPE
# pyflakes on


import testsuite_support.adalog_driver
import testsuite_support.langkit_support_driver
import testsuite_support.python_driver


class Testsuite(BaseTestsuite):
    TEST_SUBDIR = 'tests'
    DRIVERS = {
        'python': testsuite_support.python_driver.PythonDriver,
        'adalog': testsuite_support.adalog_driver.AdalogDriver,
        'langkit_support':
        testsuite_support.langkit_support_driver.LangkitSupportDriver
    }

    def add_options(self):
        self.main.add_option(
            '--with-python', default=None,
            help='If provided, use as the Python interpreter in testcases.')

        self.main.add_option(
            '--coverage', '-C', action='store_true',
            help='Enable computation of code coverage for Langkit'
        )

        #
        # Convenience options for developpers
        #

        self.main.add_option(
            '--disable-tear-up-builds', '-B', action='store_true',
            help='Disable the automatic build of Langkit_Support and Adalog'
                 ' during the testsuite tear_up step. This is used to speed up'
                 ' successive testsuite runs during development.'
        )

        # Tests update
        self.main.add_option(
            '--rewrite', '-r', action='store_true',
            help='Rewrite test baselines according to current output.'
        )

    @property
    def coverage_enabled(self):
        return self.global_env['options'].coverage

    @property
    def coverage_dir(self):
        return os.path.join(self.global_env['output_dir'], 'coverage')

    def tear_up(self):
        super(Testsuite, self).tear_up()

        if self.coverage_enabled:
            # Create a directory that we'll use to:
            #
            #   1) collect coverage data for each testcase;
            #   2) generate the HTML report.
            os.mkdir(self.coverage_dir)
            self.global_env['coverage_dir'] = self.coverage_dir

        def report(p, pname):
            if p.status != 0:
                raise RuntimeError(
                    '{} build failed (GPRbuild returned {})\n{}'.format(
                        pname, p.status, p.out
                    )
                )

        # Build Langkit_Support and Adalog so that each testcase does not try
        # to build it in parallel.
        if not self.global_env['options'].disable_tear_up_builds:
            p = Run(['gprbuild', '-p', '-f', '-P',
                     os.path.join(self.root_dir, '..', 'langkit', 'support',
                                  'langkit_support.gpr')], output=PIPE)
            report(p, "Langkit support")

    def tear_down(self):
        if self.coverage_enabled:
            # Consolidate coverage data for each testcase and generate both a
            # sumary textual report on the standard output and a detailed HTML
            # report.
            cov_files = glob.glob(os.path.join(self.coverage_dir,
                                               '*.coverage'))
            rcfile = os.path.join(self.root_dir, 'coverage.ini')

            for argv in (
                ['combine'] + cov_files,
                ['report'],
                ['html', '-d.', '--title=Langkit coverage report'],
            ):
                if argv[0] != 'combine':
                    argv.append('--rcfile=' + rcfile)
                subprocess.check_call(['coverage'] + argv,
                                      cwd=self.coverage_dir)

            html_index = os.path.join(self.coverage_dir, 'index.html')
            assert os.path.exists(html_index)
            print('Detailed HTML coverage report available at:'
                  ' {}'.format(html_index))

        super(Testsuite, self).tear_down()
