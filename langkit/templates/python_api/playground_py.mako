#! /usr/bin/env python

## vim: filetype=makopython

<%namespace name="exts" file="../extensions.mako" />

import argparse

from IPython import embed
from IPython.terminal.ipapp import load_default_config

<% short_name = ctx.short_name.lower if ctx.short_name else 'mdl' %>

import ${module_name}
import ${module_name} as ${short_name}


HEADER = """
--
-- ${module_name} playground
--

The file(s) passed as argument have been put into the u variable, or units if
there are multiple.

Enjoy!
""".strip()



class Playground(object):
    """
    Base class to regroup logic for the playground. We use a class so that
    specific languages implementations can add specific arguments and
    processing by overriding:

    - add_arguments to add arguments to the argparse.Parser instance

    - create_unit_provider to return a custom unit provider to be used by the
      AnalysisContext.
    """

    def __init__(self):
        self.parser = argparse.ArgumentParser(
            description="${module_name} playground. Analyze files passed "
            "as arguments."
        )
        self.parser.add_argument('files', nargs='+', help='Files')
        self.parser.add_argument(
            '-i', '--input-script', type=str, default='',
            help="Script to execute when playground has loaded the units"
        )

    def add_arguments(self):
        """
        Hook for subclasses to add arguments to self.parser. Default
        implementation does nothing.
        """
        pass

    def create_unit_provider(self):
        """
        Hook for subclasses to return a custom unit provider.
        Default implementation returns None.
        """
        return None

    def process_file(self, file_name):
        u = self.ctx.get_from_file(file_name)
        return u

    def main(self):
        self.add_arguments()
        self.args = self.parser.parse_args()
        self.ctx = ${module_name}.AnalysisContext(
            'utf-8', with_trivia=True,
            unit_provider=self.create_unit_provider()
        )


class ${short_name}Playground(Playground):
    ${exts.include_extension(ctx.ext('playground'))}
    pass


if __name__ == '__main__':
    pg = ${short_name}Playground()
    pg.main()
    units = {}
    for file_path in pg.args.files:
        u = pg.process_file(file_path)
        units[file_path] = u

    print HEADER
    c = load_default_config()

    if pg.args.input_script:
        execfile(pg.args.input_script)

    embed(header=HEADER, config=c, display_banner=False)
