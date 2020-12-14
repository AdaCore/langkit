#! /usr/bin/env python

from langkit.libmanage import ManageScript


class Manage(ManageScript):

    ENABLE_BUILD_WARNINGS_DEFAULT = True

    @property
    def main_programs(self):
        return super(Manage, self).main_programs | {'lkt_toolbox'}

    def create_context(self, args):
        from langkit.compile_context import CompileCtx, LibraryEntity

        from language.lexer import lkt_lexer
        from language.parser import lkt_grammar

        return CompileCtx(
            lang_name='lkt',
            short_name='lkt',
            lexer=lkt_lexer,
            grammar=lkt_grammar,
            default_unit_provider=LibraryEntity(
                'Liblktlang.Default_Provider', 'Create'
            )
        )


if __name__ == '__main__':
    Manage().run()
