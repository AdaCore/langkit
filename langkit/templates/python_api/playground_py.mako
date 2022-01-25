#! /usr/bin/env python

## vim: filetype=makopython

<%namespace name="exts" file="../extensions.mako" />

from __future__ import (absolute_import, division, print_function,
                        unicode_literals)

import argparse

from IPython import embed
from IPython.terminal.ipapp import load_default_config

<% short_name = ctx.short_name or 'mdl' %>

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

class Playground(${module_name}.App):

    def add_arguments(self):
        self.parser.add_argument(
            '-i', '--input-script', type=str, default='',
            help="Script to execute when playground has loaded the units"
        )
        super(Playground, self).add_arguments()

    def main(self):
        print(HEADER)
        c = load_default_config()

        if self.args.input_script:
            execfile(self.args.input_script)

        # Put useful values in local variables, so that they're easily
        # accessible from embed.
        units = self.units
        ctx = self.ctx
        u = self.u

        embed(header=HEADER, config=c, display_banner=False)


if __name__ == '__main__':
    Playground.run()
