import os

from e3.testsuite.driver.classic import TestAbortWithFailure
from e3.testsuite.result import FailureReason

from drivers.base_driver import BaseDriver


class LktformatDriver(BaseDriver):
    """
    Driver to run the "lktformat" program with given command line arguments.
    """

    def run(self):
        p = self.run_and_check(
            ["lktformat", *self.test_env["argv"]], catch_error=False
        )

        exp_status = self.test_env["expected_status_code"]
        if p.status != exp_status:
            raise TestAbortWithFailure(f"unexpected status code: {p.status}")

    def compute_failures(self):
        # Let the base driver check the output of lktformat itself
        result = super().compute_failures()

        # Also check the contents of reformatted (or not reformatted) Lkt
        # source files.
        for filename in os.listdir(self.test_dir()):
            if filename.endswith(".lkt"):
                baseline_file = self.test_dir(f"{filename}.baseline")
                with open(baseline_file) as f:
                    baseline = f.read()

                actual_file = self.working_dir(filename)
                with open(actual_file) as f:
                    actual = f.read()

                sub_result = self.compute_diff(
                    baseline_file=baseline_file,
                    baseline=baseline,
                    actual=actual,
                    failure_message=f"unexpected contents for '{filename}'",
                )
                if sub_result:
                    self.result.failure_reasons.add(FailureReason.DIFF)
                result += sub_result

        # Calls to compute_diff extended `self.result.log` with potential
        # diffs. Do not keep `self.result.expected`/`.out`/`.diff` since that
        # would be confusing (what output do they relate to?).
        self.result.expected = None
        self.result.out = None
        self.result.diff = None

        return result
