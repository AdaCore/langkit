#! /usr/bin/env python

from __future__ import absolute_import, division, print_function

import liblktlang as lkt
from os import path as P


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
                print("Syntax errors in {}, skipping".format(
                    P.basename(unit_name)
                ))
                for diag in unit.diagnostics:
                    print(str(diag))
                continue

            msg = "Resolving {}".format(unit_name)
            print("{}\n{}".format(msg, "=" * len(msg)))

            if self.args.print_lkt:
                print()
                print("Source:")
                print()
                print(unit.root.text)
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
                    print("Resolving type of expr {} raised an error".format(
                        node
                    ))
                    print("    {}".format(e.message))

                if node.is_a(lkt.RefId):
                    print("Id   {}".format(node))
                    print("     references {}".format(node.p_referenced_decl))
                    print()


if __name__ == '__main__':
    Resolve.run()
