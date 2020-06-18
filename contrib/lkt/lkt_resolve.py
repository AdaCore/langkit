#! /usr/bin/env python

from functools import lru_cache
from os import path as P
import re
from typing import List, Union

from langkit.diagnostics import Location
from langkit.utils.colors import Colors, col, printcol

import liblktlang as L


@lru_cache()
def splitted_text(unit: L.AnalysisUnit) -> List[str]:
    """
    Memoized function to get the splitted text of an unit. Used to not have to
    compute this every time.
    """
    return unit.text.splitlines()


def style_diagnostic_message(string: str) -> str:
    """
    Given a diagnostic message containing possible variable references
    surrounded by backticks, style those references.
    """
    return re.sub("`.*?`", lambda m: col(m.group(), Colors.BOLD), string)


def source_listing(highlight_sloc: Location, lines_after: int = 0) -> str:
    """
    Create a source listing for an error message, centered around a specific
    sloc, that will be highlighted/careted, as in the following example::

        65 | fun test(): Int = b_inst.fun_call
           |                   ^^^^^^^^^^^^^^^

    :param highlight_sloc: The source location that will allow us
        to create the specific listing.
    :param lines_after: The number of lines to print after the given sloc.
    """

    source_buffer = splitted_text(highlight_sloc.lkt_unit)

    ret = []

    line_nb = highlight_sloc.line - 1
    start_offset = highlight_sloc.column - 1
    end_offset = highlight_sloc.end_column - 1

    # Compute the width of the column needed to print line numbers
    line_nb_width = len(str(highlight_sloc.line + lines_after))

    # Precompute the format string for the listing left column
    prefix_fmt = "{{: >{}}} | ".format(line_nb_width)

    def append_line(line_nb, line):
        """
        Append a line to the source listing, given a line number and a line.
        """
        ret.append(col(prefix_fmt.format(line_nb, line),
                       Colors.BLUE + Colors.BOLD))
        ret.append(line)
        ret.append("\n")

    # Append the line containing the sloc
    append_line(line_nb, source_buffer[line_nb])

    # Append the line caretting the sloc in the line above
    caret_line = "".join("^" if start_offset <= i < end_offset else " "
                         for i in range(len(source_buffer[line_nb])))
    append_line("", col(caret_line, Colors.RED + Colors.BOLD))

    # Append following lines up to ``lines_after`` lines
    for line_nb, line in enumerate(
        source_buffer[line_nb + 1:
                      min(line_nb + lines_after + 1, len(source_buffer))],
        line_nb + 1
    ):
        append_line(line_nb, line)

    return "".join(ret)


def print_error(message: str, location: Union[Location, L.LKNode]):
    """
    Prints an error.
    """
    if isinstance(location, L.LKNode):
        location = Location.from_lkt_node(location)

    # Print the basic error (with colors if in tty)
    print(
        "{}: {}{}".format(
            col(location.gnu_style_repr(), Colors.BOLD),
            col(col("error: ", Colors.RED), Colors.BOLD),
            style_diagnostic_message(message),
        ),
    )

    # Print the source listing
    if location.lkt_unit is not None:
        print(source_listing(location))


def print_error_from_sem_result(sem_result: L.SemanticResult):
    """
    Prints an error from an lkt semantic result.

    TODO: This should be moved in langkit.diagnostics once we use LKT.
    """
    print_error(sem_result.error_message,
                Location.from_lkt_node(sem_result.node))


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
