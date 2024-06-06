"""
Check the automatic docstring additions performed when generating code.
"""

from langkit.names import Name


# Excerpts of docstrings that we intend to check in generated sources
excerpts = [
    # "Raw" docstrings above
    "Docstring for ``",

    # Derivation additions
    "This node type has the following derivations:",
    "This node type has no derivation.",

    # Python-specific additions for nodes
    "Subclass of :py:class",
]

for type_name in [
    "Foo_Node",
    "Foo_Node_Base_List",
    "Undocumented_Node",
    "Documented_Node",
    "Documented_Node_List",
    "Derived_List_Node",
]:
    t = Name(type_name)
    excerpts += [
        f":ada:ref:`{t.camel_with_underscores}`",
        f":py:class:`{t.camel}`",
        f"``foo_{t.lower}``",
        f"``{t.lower}``",
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
            if line.strip() in {"#", "--", "*", ""}:
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
