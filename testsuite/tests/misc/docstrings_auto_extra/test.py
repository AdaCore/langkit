"""
Check the automatic docstring additions performed when generating code.
"""


from langkit.dsl import ASTNode, has_abstract_list

from utils import emit_and_print_errors


class FooNode(ASTNode):
    """
    Docstring for ``FooNode``.
    """
    pass


class UndocumentedNode(FooNode):
    pass


@has_abstract_list
class DocumentedNode(FooNode):
    """
    Docstring for ``DocumentedNode``.
    """
    pass


class DerivedListNode(DocumentedNode.list):
    """
    Docstring for ``DerivedListNode``.
    """


emit_and_print_errors(lkt_file="foo.lkt")

# Excerpts of docstrings that we intend to check in generated sources
excerpts = [
    # "Raw" docstrings above
    "Docstring for ``",

    # Python-specific additions for nodes
    "Subclass of :py:class",
]

# Log docstrings
for path in [
    "build/src/libfoolang-analysis.ads",
    "build/src/libfoolang.h",
    "build/ocaml/libfoolang.mli",
    "build/python/libfoolang/__init__.py",
]:
    with open(path) as f:
        print("")
        print(f"{path}:")

        # In each file, we want to display multiple comment blocks/docstrings.
        # To make the separation between the various docstrings easy to read in
        # the test output, try to detect the separation between these blocks
        # and insert an empty line in the test output when starting a new
        # block.
        in_block = True

        for line in f.readlines():
            # Skip empty lines lines that are empty comments from our analysis.
            # This is necessary for the continuous lines detection.
            if line.strip() in {"#", "--", ""}:
                continue

            if any(e in line for e in excerpts):
                if not in_block:
                    print("")
                    in_block = True
                print(f"  {line.strip()}")
            else:
                in_block = False

print("")
print("Done")
