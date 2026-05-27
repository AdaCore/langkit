from drivers.base_driver import BaseDriver


class LktToolboxDriver(BaseDriver):
    """
    Custom driver that will automatically run the "lkt_toolbox" utility on an
    Lkt source file.
    """

    @property
    def check_only(self) -> bool:
        return self.test_env.get("check_only", False)

    @property
    def entry_points(self) -> str:
        """
        Return the names of the source files on which to run "lkt_toolbox".
        """
        return self.test_env.get("entry_points", ["test.lkt"])

    def run(self):
        base_args = ["lkt_toolbox", "--check-invalid-decls"]
        if self.check_only:
            base_args.append("--check-only")

        for filename in self.entry_points:
            self.run_and_check(
                [*base_args, filename],
                memcheck=self.memcheck_for_lkt,
                valgrind_suppressions=["gnat"],
                for_coverage=True,
            )
