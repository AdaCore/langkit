from drivers.base_driver import BaseDriver


class LangkitSupportDriver(BaseDriver):
    default_process_timeout = 300

    def run(self):
        source = self.test_env.get("main_source", "main.adb")
        self.create_project_file("p.gpr", [source])
        self.gprbuild("p.gpr")
        argv = [self.program_path(source)]
        self.run_and_check(argv, for_coverage=True, memcheck=True)
