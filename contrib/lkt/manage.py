#! /usr/bin/env python

import os.path

from langkit.common import bytes_repr
from langkit.compile_context import AdaSourceKind, CompileCtx
import langkit.config as C
from langkit.libmanage import ManageScript
import langkit.names as names
from langkit.passes import EmitterPass
from langkit.utils import PluginLoader


class Manage(ManageScript):

    ENABLE_BUILD_WARNINGS_DEFAULT = True

    @property
    def extra_main_programs(self):
        return {'lkt_toolbox'}

    def create_config(self, args):
        return C.CompilationConfig(
            lkt=None,
            library=C.LibraryConfig(
                root_directory=os.path.dirname(__file__),
                language_name=names.Name("Lkt"),
                short_name="lkt",
                standalone=True,
                defaults=C.LibraryDefaults(
                    unit_provider=C.LibraryEntity(
                        "Liblktlang.Default_Provider", "Create"
                    ),
                    unparsing_config="default_unparsing_config.json",
                ),
            ),
            mains=C.MainsConfig(
                source_dirs=["extensions/mains"],
                main_programs=["lkt_toolbox"],
            ),
            plugin_passes=["manage.generate_prelude_pass"],
        )

    def create_context(self, config):
        from language.lexer import lkt_lexer
        from language.parser import lkt_grammar

        return CompileCtx(
            config=config,
            plugin_loader=PluginLoader(config.library.root_directory),
            lexer=lkt_lexer,
            grammar=lkt_grammar,
        )

    @staticmethod
    def generate_prelude(emitter, context):
        """
        Generate the liblktlang-prelude.ads source file from prelude.lkt.
        """
        # Read the prelude as a sequence of bytes, to match the destination
        # String value.
        prelude_filename = os.path.join(
            os.path.dirname(__file__),
            "language",
            "prelude.lkt",
        )
        with open(prelude_filename, "rb") as f:
            content = f.read()

        # Format the sources
        lines = [
            "package Liblktlang.Prelude is",
            "   Content : constant String :=",
            bytes_repr(content, indent=" " * 6),
            "   ;",
            "end Liblktlang.Prelude;",
        ]

        # Write the source file and register it, so that it is referenced in
        # the generated project file.
        qual_name = ["Liblktlang", "Prelude"]
        emitter.write_ada_file(
            out_dir=emitter.src_dir,
            source_kind=AdaSourceKind.spec,
            qual_name=qual_name,
            content="\n".join(lines),
        )
        emitter.add_library_interface(
            emitter.ada_file_path(
                emitter.src_dir, AdaSourceKind.spec, qual_name
            ),
            generated=True,
        )


def generate_prelude_pass():
    return EmitterPass(
        "generate prelude inline sources", Manage.generate_prelude
    )


if __name__ == '__main__':
    Manage().run()
