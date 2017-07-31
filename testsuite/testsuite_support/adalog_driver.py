from __future__ import absolute_import, division, print_function

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
        coverage = self.global_env['options'].coverage

        source = self.test_env.get('main_source', 'main.adb')
        with open(self.working_dir('p.gpr'), 'w') as f:
            f.write("""
            with "{langkit_support}";

            project P is
                for Languages use ("Ada");
                for Source_Dirs use (".");
                for Object_Dir use ".";
                for Main use ("{main_source}");
            end P;
            """.format(
                main_source=source,
                langkit_support=os.path.join(
                    self.testsuite_dir, "..",
                    "langkit", "support", "langkit_support.gpr"
                )
            ))

        gargs = ['-p', '-Pp.gpr']
        cargs = ['-cargs', '-O0', '-g']
        if coverage:
            gargs.append('--subdirs=gnatcov')
            cargs.extend(['-fdump-scos', '-fpreserve-control-flow'])
        self.run_and_check(['gprbuild'] + gargs + cargs)

        program_name = source[:-4]
        if coverage:
            trace_file = self.coverage_file('trace')
            argv = ['gnatcov', 'run', '-o', trace_file,
                    self.working_dir('gnatcov', program_name)]
        else:
            argv = ['./{}'.format(program_name)]
        self.run_and_check(argv)
