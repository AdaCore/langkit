from drivers.base_driver import BaseDriver


class LktToolboxDriver(BaseDriver):
    """
    Custom driver that will automatically run the "lkt_toolbox" utility
    on the testcase's "test.lkt" file.
    """

    def run(self):
        self.run_and_check(
            ["lkt_toolbox", "--check-invalid-decls", "test.lkt"],
            memcheck=self.memcheck_for_lkt,
            for_coverage=True,
        )
