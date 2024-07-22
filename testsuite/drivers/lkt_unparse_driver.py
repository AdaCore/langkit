import os
import os.path

from drivers.base_driver import BaseDriver


class LktUnparseDriver(BaseDriver):
    """
    Driver that runs the "lal_unprase" program on a source file using Lkt's
    default unparsing configuration.
    """

    def run(self):
        cfg = os.path.join(
            self.langkit_root_dir,
            "contrib",
            "lkt",
            "extensions",
            "default_unparsing_config.json",
        )
        self.run_and_check(["lkt_unparse", cfg, "input.lkt"], memcheck=True)
