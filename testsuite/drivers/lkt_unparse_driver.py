import os
import os.path

from drivers.base_driver import BaseDriver


class LktUnparseDriver(BaseDriver):
    """
    Driver that runs the "lal_unprase" program on a source file using Lkt's
    default unparsing configuration.
    """

    def run(self):
        # Even though we are using the default unparsing configuration, pass
        # the JSON file explicitly so that one does not need to rebuild
        # Liblktlang in order to test a change in that configuration.
        cfg = os.path.join(
            self.langkit_root_dir,
            "lkt",
            "extensions",
            "default_unparsing_config.json",
        )
        self.run_and_check(
            ["lkt_unparse", "-c", cfg, "-A", "-C", "input.lkt"],
            memcheck=self.memcheck_for_lkt,
        )
