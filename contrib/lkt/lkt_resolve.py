#! /usr/bin/env python

from os import path as P

from langkit.diagnostics import print_error_from_sem_result
from langkit.utils.colors import Colors, col, printcol

import liblktlang as L


class Resolve(L.App):
    """
    This script will resolve every type and RefId in a LKT source file.
    """

    def add_arguments(self):
        self.parser.add_argument(
            '-p', '--print-lkt', action="store_true",
            help="Print the lkt source"
        )
        self.parser.add_argument(
            '-C', '--check-only', action="store_true",
            help="Only print diagnostics"
        )
        super(Resolve, self).add_arguments()

    def format_node(self, node: L.LKNode) -> str:
        """
        Return ``str(node)`` except for declarations from the prelude, to avoid
        sloc variability when the prelude changes.
        """
        if (
            isinstance(node, L.Decl)
            and P.basename(node.unit.filename) == "__prelude"
        ):
            return f'<{node.kind_name} prelude: "{node.p_full_name}">'
        else:
            return str(node)

    def main(self) -> None:
        for unit_name, unit in self.units.items():
            if unit.diagnostics:
                printcol("Syntax errors in {}, skipping".format(
                    P.basename(unit_name)
                ), Colors.RED)
                for diag in unit.diagnostics:
                    print(str(diag))
                continue

            if not self.args.check_only:
                msg = "Resolving {}".format(unit_name)
                print("{}\n{}".format(msg, "=" * len(msg)))

            if self.args.print_lkt:
                print()
                print("Source:")
                print()
                for i, l in enumerate(unit.root.text.splitlines()):
                    print("{: <3}: {}".format(i + 1, l))
                print()

            diags = unit.root.p_check_legality

            if self.args.check_only:
                for diag in diags:
                    print_error_from_sem_result(diag)
            else:
                results = unit.root.p_check_semantic
                for result in results.results:
                    if result.error_message:
                        print_error_from_sem_result(result)
                    elif result.result_type is not None:
                        print("Expr {}".format(self.format_node(result.node)))
                        print("     has type {}"
                              .format(self.format_node(result.result_type)))
                        print()
                    elif result.result_ref is not None:
                        print("Id   {}".format(self.format_node(result.node)))
                        res = ("     references {}"
                               .format(self.format_node(result.result_ref)))
                        print(col(res, Colors.RED)
                              if result.result_ref is None else res)
                        print()


if __name__ == '__main__':
    Resolve.run()
