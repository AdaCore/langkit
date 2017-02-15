#! /usr/bin/env python

## vim: filetype=makopython

import argparse
from IPython import embed
from IPython.terminal.ipapp import load_default_config
import ${module_name} as mdl

HEADER = """
--
-- ${module_name} playground
--

The file(s) passed as argument have been put into the u variable, or units if
there are multiple.

Enjoy !
""".strip()

c = mdl.AnalysisContext('utf-8')

parser = argparse.ArgumentParser(
    description="Libadalang playground. Analyze files passed as argument"
)
parser.add_argument('files', nargs='+', help='Files')
parser.add_argument('--semres', action='store_true')
parser.add_argument('--trivia', action='store_true')
args = parser.parse_args()


def process_file(file_name):
    u = c.get_from_file(file_name, with_trivia=args.trivia)
    if args.semres:
        u.populate_lexical_env()
    return u


if __name__ == '__main__':
    if len(args.files) == 1:
        u = process_file(args.files[0])
    else:
        units = {}
        for file_path in args.files:
            u = process_file(file_path)
            units[file_path] = u

print HEADER
c = load_default_config()
embed(header=HEADER, config=c, display_banner=False)
