import os

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

    def tear_up(self):

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
