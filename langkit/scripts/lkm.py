import argparse

import langkit.config as C
from langkit.libmanage import ManageScript


class Manage(ManageScript):

    @staticmethod
    def add_common_args(subparser: argparse.ArgumentParser) -> None:
        subparser.add_argument(
            "--config",
            "-c",
            default="langkit.yaml",
            help="Configuration file to use. By default, use 'langkit.yaml' in"
            " the current directory.",
        )
        ManageScript.add_common_args(subparser)

    def create_config(self, args: argparse.Namespace) -> C.CompilationConfig:
        return self.load_yaml_config(args.config)


def main(argv: list[str] | None = None) -> None:
    Manage().run(argv)


def main_no_exit(argv: list[str] | None = None) -> int:
    return Manage().run_no_exit(argv)


if __name__ == "__main__":
    main()
