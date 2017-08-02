from __future__ import absolute_import, division, print_function

from testsuite_support.base_driver import BaseDriver, catch_test_errors


class LangkitSupportDriver(BaseDriver):
    TIMEOUT = 300

    @catch_test_errors
    def tear_up(self):
        super(LangkitSupportDriver, self).tear_up()

    @catch_test_errors
    def run(self):
        source = self.test_env.get('main_source', 'main.adb')
        self.create_project_file('p.gpr', [source])
        self.gprbuild('p.gpr')
        self.run_and_check([self.program_path(source)], for_coverage=True)
