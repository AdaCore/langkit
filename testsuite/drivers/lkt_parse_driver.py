from drivers.base_driver import BaseDriver


class LktParseDriver(BaseDriver):
    """
    Driver to run the "lkt_parse" test program on the "test.lkt" file.
    """

    def run(self):
        self.run_and_check(
            ["lkt_parse", "-f", "test.lkt"],
            memcheck=self.memcheck_for_lkt,
            for_coverage=True,
        )
