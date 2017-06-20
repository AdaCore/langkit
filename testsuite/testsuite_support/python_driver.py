from __future__ import absolute_import, division, print_function

import os
import os.path
import sys

from testsuite_support.base_driver import BaseDriver, catch_test_errors


class PythonDriver(BaseDriver):
    TIMEOUT = 300

    #
    # Driver entry poins
    #

    @catch_test_errors
    def tear_up(self):
        super(PythonDriver, self).tear_up()
        self.check_file('test.py')

        # Make the common Python modules available from the testcase script
        self.add_path(b'PYTHONPATH', self.support_dir)
        self.add_path(b'PYTHONPATH', self.langkit_root_dir)

    @catch_test_errors
    def run(self):
        # The "test.py" script will not import a generated library, however
        # another spawned script could: provide the path to the interpreter in
        # the environment so it can use it.
        derived_env = dict(os.environ)
        derived_env[b'PYTHON_INTERPRETER'] = self.python_interpreter

        # If code coverage is requested, run the test script under the
        # "coverage" program.
        if self.global_env['options'].coverage:
            # Consider all Langkit Python source files, except modules which
            # are currently not tested at all.
            source = os.path.join(self.langkit_root_dir, 'langkit')
            omit = [
                os.path.join('langkit', 'fix_annotate_fields_types.py'),
                os.path.join('langkit', 'gdb', '*'),
                os.path.join('langkit', 'setup.py'),
                os.path.join('langkit', 'stylechecks', '*'),
            ]

            argv = [
                'coverage', 'run', '--branch', '--source={}'.format(source),
                '--omit={}'.format(','.join(
                    os.path.join(self.langkit_root_dir, pattern)
                    for pattern in omit
                ))
            ]
            derived_env['COVERAGE_FILE'] = os.path.join(
                self.global_env['coverage_dir'],
                self.test_env['test_name'] + '.coverage'
            )
        else:
            argv = [sys.executable]

        self.run_and_check(argv + ['test.py'], derived_env)

    @property
    def support_dir(self):
        """
        Return the absolute path to the directory for support Python modules.
        """
        return os.path.join(self.testsuite_dir, b'python_support')
