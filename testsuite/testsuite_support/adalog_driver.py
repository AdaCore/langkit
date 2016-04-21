import os
import os.path

from testsuite_support.base_driver import (
    BaseDriver, catch_test_errors,
)


class AdalogDriver(BaseDriver):
    TIMEOUT = 300

    #
    # Driver entry poins
    #

    @catch_test_errors
    def tear_up(self):
        super(AdalogDriver, self).tear_up()

    @catch_test_errors
    def run(self):
        source = self.test_env.get('main_source', 'main.adb')
        with open(self.working_dir('p.gpr'), 'w') as f:
            f.write("""
            with "{adalog}";

            project P is
                for Languages use ("Ada");
                for Source_Dirs use (".");
                for Object_Dir use ".";
                for Main use ("{main_source}");
            end P;
            """.format(
                main_source=source,
                adalog=os.path.join(self.testsuite_dir, "..", "adalog",
                                    "adalog.gpr")
            ))

        self.run_and_check(['gprbuild', '-p', '-P', 'p.gpr'])
        self.run_and_check(['./{}'.format(source[:-4])])
