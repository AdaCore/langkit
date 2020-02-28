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

            for node in unit.root.finditer(lambda _: True):
                try:
                    if node.is_a(lkt.Expr) and node.p_is_regular_expr:
                        expr_type = node.p_expr_type
                        if expr_type.error_message != "":
                            print("{}{}".format(node.full_sloc_image,
                                                expr_type.error_message))
                            print()
                        else:
                            print("Expr {}".format(node))
                            print("     has type {}"
                                  .format(expr_type.expr_type))
                            print()
                except lkt.PropertyError as e:
                    printcol(
                        "Resolving type of expr {} raised an error"
                        .format(node),
                        Colors.RED,
                    )
                    print("    {}".format(e.message))

                if node.is_a(lkt.RefId):
                    print("Id   {}".format(node))
                    refd = node.p_referenced_decl
                    res = "     references {}".format(node.p_referenced_decl)
                    print(col(res, Colors.RED) if refd is None else res)
                    print()


if __name__ == '__main__':
    Resolve.run()
