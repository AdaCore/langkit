#! /usr/bin/env python

from langkit.libmanage import ManageScript


class Manage(ManageScript):

    ENABLE_BUILD_WARNINGS_DEFAULT = True

    def create_context(self, args):
        from langkit.compile_context import CompileCtx

        from language.lexer import python_lexer
        from language.parser import python_grammar

        return CompileCtx(lang_name='Python',
                          lexer=python_lexer,
                          grammar=python_grammar,
                          standalone=True)


if __name__ == '__main__':
    Manage().run()
