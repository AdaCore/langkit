import os.path
import subprocess

from drivers.base_driver import BaseDriver


class LktUnparseDriver(BaseDriver):
    """
    Driver that runs the "lal_unprase" program on a source file using Lkt's
    default unparsing configuration.
    """

    @classmethod
    def adjust_dag_dependencies(cls, env, dag):
        # Even though we are using the default unparsing configuration, pass
        # the "current config" explicitly so that one does not need to rebuild
        # Liblktlang in order to test a change in that configuration.
        #
        # Since the unparsing engine can read only JSON files, this means that
        # we need to translate the Lkt config to JSON first, so if we plan to
        # run Lkt unparsing tests, first translate the Lkt configuration to
        # JSON.
        tests = [
            fragment
            for fragment in dag.vertex_data.values()
            if isinstance(fragment.driver, cls)
        ]
        if not tests:
            return

        lkt_config = os.path.join(
            env.langkit_root_dir,
            "lkt",
            "extensions",
            "default_unparsing_config.lkt",
        )
        env.lkt_unparsing_config_json = os.path.join(
            env.working_dir, "lkt_default_unparsing_config.json"
        )

        subprocess.check_call(
            [
                env.langkit_python_interpreter,
                "-m",
                "langkit.scripts.unparsing2lkt",
                "--to-json",
                "--output",
                env.lkt_unparsing_config_json,
                lkt_config,
            ]
        )

    def run(self):
        self.run_and_check(
            [
                "lkt_unparse",
                "-c",
                self.env.lkt_unparsing_config_json,
                "-A",
                "-C",
                "input.lkt",
            ],
            memcheck=self.memcheck_for_lkt,
        )
