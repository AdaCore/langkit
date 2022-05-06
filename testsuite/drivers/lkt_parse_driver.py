from drivers.base_driver import BaseDriver


class LktParseDriver(BaseDriver):
    """
    Driver to run the "lkt_parse" test program on the "test.lkt" file.
    """

    def run(self):
        self.run_and_check(
            ["lkt_parse", "-f", "test.lkt"],

            # We always build Langkit for 64-bit platforms, so when we test it
            # to target 32-bit, we cannot use Valgrind (32-bit) to memcheck
            # Langkit binaries (64-bit).
            memcheck=self.env.build.cpu.bits == 64,

            for_coverage=True,
        )
