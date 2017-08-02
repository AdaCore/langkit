from __future__ import absolute_import, division, print_function

import os
import os.path

from testsuite_support.base_driver import BaseDriver, catch_test_errors


class LangkitSupportDriver(BaseDriver):
    TIMEOUT = 300

    @catch_test_errors
    def tear_up(self):
        super(LangkitSupportDriver, self).tear_up()

    @catch_test_errors
    def run(self):
        source = self.test_env.get('main_source', 'main.adb')
        with open(self.working_dir('p.gpr'), 'w') as f:
            f.write("""
            with "{lk_support}";

            project P is
                for Languages use ("Ada");
                for Source_Dirs use (".");
                for Object_Dir use ".";
                for Main use ("{main_source}");
            end P;
            """.format(
                main_source=source,
                lk_support=os.path.join(self.testsuite_dir, "..", "langkit",
                                        "support", "langkit_support.gpr")
            ))

        self.run_and_check(['gprbuild', '-p', '-P', 'p.gpr', '-gnata',
                            '-cargs', '-O0', '-g'])
        self.run_and_check(['./{}'.format(source[:-4])])
