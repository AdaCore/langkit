#! /usr/bin/env python

from __future__ import absolute_import, division, print_function

from os import path as P

from langkit.utils.colors import Colors, col, printcol

import liblktlang as lkt


class Resolve(lkt.App):
    """
    This script will resolve every type and RefId in a LKT source file.
    """

    def add_arguments(self):
        self.parser.add_argument(
            '-p', '--print-lkt', action="store_true",
            help="Print the lkt source"
        )
        super(Resolve, self).add_arguments()

    def main(self):
        for unit_name, unit in self.units.items():
            if unit.diagnostics:
                printcol("Syntax errors in {}, skipping".format(
                    P.basename(unit_name)
                ), Colors.RED)
                for diag in unit.diagnostics:
                    print(str(diag))
                continue

            msg = "Resolving {}".format(unit_name)
            print("{}\n{}".format(msg, "=" * len(msg)))

            if self.args.print_lkt:
                print()
                print("Source:")
                print()
                for i, l in enumerate(unit.root.text.splitlines()):
                    print("{: <3}: {}".format(i + 1, l))
                print()

            def print_error(res):
                printcol(
                    "{}{}".format(res.node.full_sloc_image, res.error_message),
                    Colors.RED
                )
                print()

            results = unit.root.p_check_semantic
            for result in results.results:
                if result.error_message:
                    print_error(result)
                elif result.result_type is not None:
                    print("Expr {}".format(result.node))
                    print("     has type {}".format(result.result_type))
                    print()
                elif result.result_ref is not None:
                    print("Id   {}".format(result.node))
                    res = "     references {}".format(result.result_ref)
                    print(col(res, Colors.RED)
                          if result.result_ref is None else res)
                    print()


if __name__ == '__main__':
    Resolve.run()
