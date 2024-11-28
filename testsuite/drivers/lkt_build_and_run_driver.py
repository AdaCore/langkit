import os
import os.path

from drivers.python_driver import PythonDriver


class LktBuildAndRunDriver(PythonDriver):
    """
    Driver to generate/build the "test.lkt" language specification in the
    current directory and optionally build and run test programs
    (Ada/C/Python/OCaml/Java) with the generated library.

    In addition to the common "test.yaml" keys supported in the Langkit
    testsuite, this driver accepts keys that match arguments of the
    python_suport/utils.py:build_and_run function.

    The "post_scripts" key is an optional list of Python scripts to run after
    the compilation/main execution cycles have completed.
    """

    @property
    def mandatory_files(self):
        return []

    @property
    def script_and_args(self):
        result = [os.path.join(self.support_dir, "lkt_build_and_run.py")]

        def convert_default(value):
            return value

        def convert_main(main):
            if isinstance(main, str):
                encoding = self.default_encoding
                argv = main
            else:
                encoding = main["encoding"]
                argv = main["argv"]
            return f"{encoding}:{argv}"

        def opt(key):
            return "--" + key.replace("_", "-")

        def handle_flag(key):
            if self.test_env.get(key):
                result.append(opt(key))

        def handle_single(key, convert=convert_default):
            if self.test_env.get(key):
                result.append(f"{opt(key)}={convert(self.test_env[key])}")

        def handle_multiple(key, convert=convert_default):
            if self.test_env.get(key):
                result.extend(
                    f"{opt(key)}={convert(v)}" for v in self.test_env[key]
                )

        handle_single("default_unparsing_config")

        handle_single("default_unit_provider")
        handle_single("symbol_canonicalizer")
        handle_flag("show_property_logging")
        handle_single("version")
        handle_single("build_date")
        handle_multiple("property_exceptions")
        handle_single("symbol_canonicalizer")
        if "cache_collection" in self.test_env:
            config = self.test_env["cache_collection"]
            result += [
                "--cache-collection",
                str(config['threshold_increment']),
                config['decision_heuristic'] or "null",
            ]

        handle_single("py_script")
        handle_single("py_args")
        handle_single("ocaml_main", convert=convert_main)
        handle_single("java_main", convert=convert_main)
        handle_single("ni_main", convert=convert_main)
        handle_multiple("gpr_mains", convert=convert_main)

        handle_multiple("post_scripts")

        return result
