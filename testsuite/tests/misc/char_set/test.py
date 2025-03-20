"""
Test that the low-level lexing API in Ada works as expected.
"""

from langkit.lexer.char_set import CharSet


def check_ranges(label, cs):

    def format_char(char):
        return (
            chr(char)
            if ord(" ") < char and char <= ord("~")
            else "\\U+{:04X}".format(char)
        )

    print("== {} ==".format(label))
    print(
        " ".join(
            "{}-{}".format(format_char(l), format_char(h))
            for l, h in cs.ranges
        )
    )
    print("")


check_ranges("Single", CharSet("a"))
check_ranges("Adjacent 2 singles", CharSet("a", "b"))
check_ranges("Non-adjacent 2 singles", CharSet("a", "c"))
check_ranges("Reverted non-adjacent 2 singles", CharSet("c", "a"))
check_ranges("Adjacent 3 singles", CharSet("a", "c", "b"))

check_ranges("Empty range", CharSet(("a", "c"), ("d", "c")))
check_ranges("Redundant single", CharSet(("a", "c"), "b"))
check_ranges("Non-adjacent ranges", CharSet(("i", "o"), ("a", "c")))

for c in ("h", "i", "j", "k"):
    check_ranges(
        "Adjacent ranges - {}".format(c), CharSet(("i", "o"), ("a", c))
    )

check_ranges("Overlapping ranges (1)", CharSet(("i", "o"), ("a", "o")))
check_ranges("Nested range", CharSet(("i", "o"), ("a", "p")))

for c in ("i", "j", "k"):
    check_ranges("Overlappingranges (2)", CharSet(("i", "o"), (c, "p")))

check_ranges("Negation", CharSet(("\x00", "\x10"), ("b", "y")).negation)


print("Done")
