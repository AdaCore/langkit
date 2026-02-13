from drivers.base_driver import BaseDriver


class LktToolboxDriver(BaseDriver):
    """
    Custom driver that will automatically run the "lkt_toolbox" utility on an
    Lkt source file.
    """

    @property
    def entry_point(self) -> str:
        """
        Return the name of the source file on which to run "lkt_toolbox".
        """
        return self.test_env.get("entry_point", "test.lkt")

    def run(self):
        self.run_and_check(
            ["lkt_toolbox", "--check-invalid-decls", self.entry_point],
            memcheck=self.memcheck_for_lkt,
            valgrind_suppressions=["gnat"],
            for_coverage=True,
        )
