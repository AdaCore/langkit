import os.path

import yaml

import langkit.config as C
from langkit.diagnostics import Location, diagnostic_context
from langkit.libmanage import ManageScript


class Manage(ManageScript):
    def create_config(self):
        filename = os.path.join(os.path.dirname(__file__), "langkit.yaml")
        with open(filename) as f:
            json = yaml.safe_load(f)
        with diagnostic_context(Location.nowhere):
            return C.CompilationConfig.from_json("", json)


if __name__ == '__main__':
    Manage().run()
