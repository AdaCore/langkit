#! /usr/bin/env python

import os.path

from langkit.common import bytes_repr
from langkit.emitter import AdaSourceKind, ada_file_path, write_ada_file
from langkit.libmanage import ManageScript
import langkit.names as names
from langkit.passes import EmitterPass


class Manage(ManageScript):

    ENABLE_BUILD_WARNINGS_DEFAULT = True

    @property
    def main_programs(self):
        return super(Manage, self).main_programs | {'lkt_toolbox'}

    @property
    def extra_code_emission_passes(self):
        return [
            EmitterPass("generate prelude inline sources",
                        self.generate_prelude),
        ]

    def create_context(self, args):
        from langkit.compile_context import CompileCtx, LibraryEntity

        from language.lexer import lkt_lexer
        from language.parser import lkt_grammar

        return CompileCtx(
            lang_name='Lkt',
            short_name='Lkt',
            lexer=lkt_lexer,
            grammar=lkt_grammar,
            default_unit_provider=LibraryEntity(
                'Liblktlang.Default_Provider', 'Create'
            )
        )

    def generate_prelude(self, emitter, context):
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
        qual_name = [names.Name("Liblktlang"), names.Name("Prelude")]
        write_ada_file(
            out_dir=emitter.src_dir,
            source_kind=AdaSourceKind.spec,
            qual_name=qual_name,
            content="\n".join(lines),
            post_process=emitter.post_process_ada,
        )
        emitter.add_library_interface(
            ada_file_path(emitter.src_dir, AdaSourceKind.spec, qual_name),
            generated=True,
        )


if __name__ == '__main__':
    Manage().run()
