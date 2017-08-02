from __future__ import absolute_import, division, print_function

from testsuite_support.base_driver import BaseDriver, catch_test_errors


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
        self.create_project_file('p.gpr', [source])

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
