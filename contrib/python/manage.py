#! /usr/bin/env python

import os.path

from langkit.compile_context import CompileCtx
import langkit.config as C
from langkit.libmanage import ManageScript
import langkit.names as names
from langkit.utils import PluginLoader


class Manage(ManageScript):

    ENABLE_BUILD_WARNINGS_DEFAULT = True

    def create_config(self, args):
        return C.CompilationConfig(
            lkt=None,
            library=C.LibraryConfig(
                root_directory=os.path.dirname(__file__),
                language_name=names.Name("Python"),
                standalone=True,
            ),
        )

    def create_context(self, config):
        from language.lexer import python_lexer
        from language.parser import python_grammar

        return CompileCtx(
            config=config,
            plugin_loader=PluginLoader(config.library.root_directory),
            lexer=python_lexer,
            grammar=python_grammar,
        )


if __name__ == '__main__':
    Manage().run()
