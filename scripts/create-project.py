#! /usr/bin/env python

from __future__ import absolute_import, division, print_function

import argparse
import os


parser = argparse.ArgumentParser(
    description='Generate a skeleton project for a Langkit-based language'
                ' engine'
)
parser.add_argument('language', help='Name of the target language')


def generate(lang_name):
    lang_name_slug = lang_name.lower()

    template_args = {
        'lang_name': lang_name,
        'lang_name_repr': repr(lang_name),
        'lang_name_slug': lang_name_slug,
    }

    # Create the directories...
    os.mkdir(lang_name_slug)
    os.chdir(lang_name_slug)
    os.mkdir('language')

    # ... and the 4 initial files.
    for filename, template in [
        ('manage.py', MANAGE_TEMPLATE),
        (os.path.join('language', '__init__.py'), ''),
        (os.path.join('language', 'lexer.py'), LEXER_TEMPLATE),
        (os.path.join('language', 'parser.py'), PARSER_TEMPLATE),
    ]:
        with open(filename, 'w') as f:
            f.write(template.format(**template_args))

    os.chmod('manage.py', 0755)


MANAGE_TEMPLATE = '''#! /usr/bin/env python

import os

from langkit.libmanage import ManageScript


class Manage(ManageScript):
    def create_context(self, args):
        from langkit.compile_context import CompileCtx

        from language.lexer import {lang_name_slug}_lexer
        from language.parser import {lang_name_slug}_grammar

        return CompileCtx(lang_name={lang_name_repr},
                          lexer={lang_name_slug}_lexer,
                          grammar={lang_name_slug}_grammar)

if __name__ == '__main__':
    Manage().run()
'''


LEXER_TEMPLATE = '''\
from langkit.lexer import Lexer, LexerToken, Literal, WithText


class Token(LexerToken):
    Example = WithText()

{lang_name_slug}_lexer = Lexer(Token)
{lang_name_slug}_lexer.add_rules(
    (Literal("example"), Token.Example),
)
'''


PARSER_TEMPLATE = '''\
from langkit.dsl import ASTNode, abstract
from langkit.parsers import Grammar


@abstract
class {lang_name}Node(ASTNode):
    """
    Root node class for {lang_name} AST nodes.
    """
    pass

class ExampleNode({lang_name}Node):
    pass

{lang_name_slug}_grammar = Grammar('main_rule')
{lang_name_slug}_grammar.add_rules(
    main_rule=ExampleNode('example')
)
'''


if __name__ == '__main__':
    generate(parser.parse_args().language)
